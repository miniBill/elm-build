module ElmCodegen exposing (engine)

import Ansi.Color.Extra
import BackendTask exposing (BackendTask)
import BackendTask.Extra
import BackendTask.File
import BackendTask.Glob as Glob exposing (Glob)
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Types exposing (Block(..), Engine(..))


engine :
    { flagsFrom : Maybe String
    , outputs : List String
    }
    -> Engine
engine config =
    let
        localHelperFilesTask :
            BackendTask
                FatalError
                (List
                    { path : String
                    , moduleName : List String
                    }
                )
        localHelperFilesTask =
            BackendTask.File.jsonFile
                (Decode.at
                    [ "codegen-helpers", "local" ]
                    (Decode.list Decode.string)
                )
                "codegen/elm.codegen.json"
                |> BackendTask.allowFatal
                |> BackendTask.Extra.log Ansi.Color.Extra.gray
                    "Local helpers"
                    (String.join ", ")
                |> BackendTask.andThen
                    (BackendTask.Extra.combineMap getModulesInDirectory)
                |> BackendTask.map List.concat
                |> BackendTask.map2
                    (\imports ->
                        List.filter
                            (\localHelperFile ->
                                List.any
                                    (\import_ ->
                                        import_.moduleName
                                            == ("Gen" :: localHelperFile.moduleName)
                                    )
                                    imports
                            )
                    )
                    importsTask
                |> BackendTask.Extra.log Ansi.Color.Extra.gray
                    "Local helper modules (only used ones)"
                    (String.join ", " << List.map (String.join "." << .moduleName))

        importsTask :
            BackendTask
                FatalError
                (List { moduleName : List String })
        importsTask =
            getModulesInDirectory "codegen"
                |> BackendTask.andThen
                    (\paths ->
                        paths
                            |> List.filterMap
                                (\{ path } ->
                                    if
                                        String.startsWith "codegen/elm-stuff/" path
                                            || String.startsWith "codegen/Gen/" path
                                    then
                                        Nothing

                                    else
                                        BackendTask.File.rawFile path
                                            |> BackendTask.allowFatal
                                            |> Just
                                )
                            |> BackendTask.combine
                    )
                |> BackendTask.map
                    (\raws ->
                        raws
                            |> List.concatMap
                                (\raw ->
                                    raw
                                        |> String.split "\n"
                                        |> List.filterMap
                                            (\line ->
                                                if String.startsWith "import " line then
                                                    line
                                                        |> String.split " "
                                                        |> List.drop 1
                                                        |> List.head
                                                        |> Maybe.map
                                                            (\name ->
                                                                { moduleName = String.split "." name }
                                                            )

                                                else
                                                    Nothing
                                            )
                                )
                    )

        flagFilesTask : BackendTask FatalError (List String)
        flagFilesTask =
            case config.flagsFrom of
                Nothing ->
                    BackendTask.succeed []

                Just path ->
                    Glob.succeed identity
                        |> Glob.captureFilePath
                        |> Glob.match (Glob.literal path)
                        |> Glob.match (Glob.literal "/")
                        |> Glob.match Glob.recursiveWildcard
                        |> Glob.toBackendTask

        installName : String
        installName =
            "elm_codegen_install"

        runName : String
        runName =
            "elm_codegen_run"

        basicsBindings : String
        basicsBindings =
            "codegen/Gen/Basics.elm"
    in
    BackendTask.map3
        (\flagFiles localHelperFiles imports ->
            [ Pool
                { name = installName
                , depth = 1
                }
            , Rule
                { name = installName
                , commands =
                    [ [ "yarn", "elm-codegen", "install" ] ]
                , pool = Just installName
                }
            , Build
                { rule = installName
                , inputs = List.map .path localHelperFiles
                , outputs =
                    basicsBindings
                        :: List.map
                            (\{ moduleName } -> moduleNameToGenPath moduleName)
                            localHelperFiles
                }
            , Pool
                { name = runName
                , depth = 1
                }
            , Rule
                { name = runName
                , commands =
                    let
                        maybeFlags : List String
                        maybeFlags =
                            case config.flagsFrom of
                                Just flags ->
                                    [ "--flags-from"
                                    , flags
                                    ]

                                Nothing ->
                                    []
                    in
                    [ [ "yarn", "elm-codegen", "run" ] ++ maybeFlags
                    , [ "elm-format", "--yes", "generated" ]
                    ]
                , pool = Just runName
                }
            , Build
                { rule = runName
                , inputs = basicsBindings :: flagFiles ++ List.filterMap importToPath imports
                , outputs = config.outputs
                }
            ]
        )
        flagFilesTask
        localHelperFilesTask
        importsTask
        |> Engine


importToPath : { a | moduleName : List String } -> Maybe String
importToPath { moduleName } =
    case moduleName of
        "Gen" :: rest ->
            Just <| moduleNameToGenPath rest

        _ ->
            Nothing


moduleNameToGenPath : List String -> String
moduleNameToGenPath moduleName =
    "codegen/Gen/" ++ String.join "/" moduleName ++ ".elm"


getModulesInDirectory : String -> BackendTask FatalError (List { moduleName : List String, path : String })
getModulesInDirectory dir =
    let
        glob : Glob { path : String, moduleName : List String }
        glob =
            Glob.succeed
                (\path components lastComponent ->
                    { path = path
                    , moduleName = components ++ [ lastComponent ]
                    }
                )
                |> Glob.captureFilePath
                |> Glob.match
                    (Glob.literal
                        (if String.endsWith "/" dir then
                            dir

                         else
                            dir ++ "/"
                        )
                    )
                |> Glob.capture Glob.recursiveWildcard
                |> Glob.match (Glob.literal "/")
                |> Glob.capture Glob.wildcard
                |> Glob.match (Glob.literal ".elm")
    in
    Glob.toBackendTask glob
