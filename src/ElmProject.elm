module ElmProject exposing (ElmProject, fromPath, eval)

{-| Evaluate and cache Elm expressions.

Given an Elm expression like `"SampleValue.greeting"`, this module:

1.  Derives the cache key from the expression and its transitive source dependencies
2.  Generates a wrapper module that outputs the value via a port
3.  Compiles with `elm make` and runs with `node`
4.  Caches the result so subsequent evaluations are instant

-}

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BackendTask.Glob as Glob
import Cache exposing (FileOrDirectory)
import DepGraph
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import Path exposing (Path)
import Set exposing (Set)


type ElmProject
    = ElmProject
        { projectDir : Path
        , sourceDirectories : List String
        , inputsByPath : Dict String ( Path, Cache.Monad FileOrDirectory )
        , depGraph : DepGraph.Graph
        }


{-| Initialize an ElmProject from a project directory.

Globs for all `.elm` files under `src/`, reads their contents for
dependency analysis, builds a dependency graph, and hashes all inputs.

-}
fromPath : Path -> BackendTask FatalError ElmProject
fromPath projectDir =
    let
        p : String
        p =
            Path.toString projectDir

        sourceDirectories : List String
        sourceDirectories =
            [ "src", "codegen" ]
    in
    Glob.fromStringWithOptions
        (let
            o : Glob.Options
            o =
                Glob.defaultOptions
         in
         { o | include = Glob.OnlyFiles }
        )
        (p ++ "/src/**/*.elm")
        |> BackendTask.andThen
            (\files ->
                let
                    elmFiles : List String
                    elmFiles =
                        files
                            |> List.filter (\f -> not (String.contains "elm-stuff" f))
                            |> List.sort
                in
                elmFiles
                    |> List.map
                        (\filePath ->
                            File.rawFile filePath
                                |> BackendTask.allowFatal
                                |> BackendTask.map (\content -> ( filePath, content ))
                        )
                    |> BackendTask.sequence
                    |> BackendTask.andThen
                        (\fileContents ->
                            let
                                depGraph : DepGraph.Graph
                                depGraph =
                                    DepGraph.buildGraph
                                        { sourceDirectories = sourceDirectories
                                        , files =
                                            fileContents
                                                |> List.map
                                                    (\( filePath, content ) ->
                                                        { filePath = stripDotSlash filePath
                                                        , content = content
                                                        }
                                                    )
                                        }

                                allPaths : List Path
                                allPaths =
                                    (elmFiles ++ [ p ++ "/elm.json" ])
                                        |> List.sort
                                        |> List.map Path.path
                            in
                            Cache.inputs allPaths
                                |> BackendTask.map
                                    (\sourceInputs ->
                                        ElmProject
                                            { projectDir = projectDir
                                            , sourceDirectories = sourceDirectories
                                            , inputsByPath =
                                                sourceInputs
                                                    |> List.map
                                                        (\( pathVal, monad ) ->
                                                            ( stripDotSlash (Path.toString pathVal)
                                                            , ( pathVal, monad )
                                                            )
                                                        )
                                                    |> Dict.fromList
                                            , depGraph = depGraph
                                            }
                                    )
                        )
            )


{-| Evaluate an Elm expression and cache the result.

The expression must be of the form `"ModuleName.valueName"` where
the value has type `String`.

The result is cached based on the transitive source dependencies of
the module, so changing an unrelated file won't cause re-evaluation.

-}
eval : ElmProject -> String -> (FileOrDirectory -> Cache.Monad a) -> Cache.Monad a
eval (ElmProject project) expression k =
    case parseExpression expression of
        Nothing ->
            Cache.fail ("Invalid expression: " ++ expression ++ " (expected \"ModuleName.valueName\")")

        Just ( moduleName, valueName ) ->
            case DepGraph.moduleNameToFilePath project.depGraph moduleName of
                Nothing ->
                    Cache.fail ("Module not found in project: " ++ moduleName)

                Just sourceFilePath ->
                    let
                        -- Get transitive deps of the target module
                        transDeps : Set String
                        transDeps =
                            DepGraph.transitiveDeps project.depGraph sourceFilePath

                        -- Filter inputsByPath to relevant deps + elm.json,
                        -- keeping the normalized path (no "./" prefix) for directory layout
                        relevantInputs : List ( String, Cache.Monad FileOrDirectory )
                        relevantInputs =
                            project.inputsByPath
                                |> Dict.toList
                                |> List.filter
                                    (\( normalizedPath, _ ) ->
                                        Set.member normalizedPath transDeps
                                            || normalizedPath == "elm.json"
                                    )
                                |> List.map (\( normalizedPath, ( _, monad ) ) -> ( normalizedPath, monad ))

                        -- Generate the wrapper module source
                        wrapperSource : String
                        wrapperSource =
                            generateWrapper moduleName valueName

                        -- Shell script: ensure source dirs exist, compile + run
                        mkdirs : String
                        mkdirs =
                            project.sourceDirectories
                                |> List.map (\d -> "mkdir -p " ++ d)
                                |> String.join " && "

                        script : String
                        script =
                            mkdirs
                                ++ " && elm make --output=elm.js src/EvalWrapper__.elm 1>&2 && node -e \"const {Elm}=require('./elm.js');Elm.EvalWrapper__.init().ports.evalOutput__.subscribe(v=>{process.stdout.write(v);process.exit(0)})\""
                    in
                    Cache.do (Cache.writeFile wrapperSource Cache.succeed) <| \wrapperHash ->
                    -- Resolve all relevant inputs and combine with wrapper into a project directory
                    Cache.do
                        (relevantInputs
                            |> List.map
                                (\( normalizedPath, monad ) ->
                                    Cache.do monad <| \hash ->
                                    Cache.succeed { filename = Path.path normalizedPath, hash = hash }
                                )
                            |> Cache.sequence
                            |> Cache.andThen
                                (\sourceFiles ->
                                    Cache.combine
                                        (sourceFiles
                                            ++ [ { filename = Path.path "src/EvalWrapper__.elm", hash = wrapperHash } ]
                                        )
                                )
                        )
                    <| \projectDirHash ->
                    Cache.commandInWritableDirectory "sh" [ "-c", script ] projectDirHash k


{-| Parse "ModuleName.valueName" into its parts.
-}
parseExpression : String -> Maybe ( String, String )
parseExpression expr =
    let
        trimmed =
            String.trim expr
    in
    case String.split "." trimmed |> List.reverse of
        valueName :: modulePartsReversed ->
            if List.isEmpty modulePartsReversed then
                Nothing

            else
                let
                    moduleName =
                        modulePartsReversed |> List.reverse |> String.join "."
                in
                if String.isEmpty valueName || String.isEmpty moduleName then
                    Nothing

                else
                    Just ( moduleName, valueName )

        _ ->
            Nothing


{-| Generate a port module that evaluates and outputs the given expression.
-}
generateWrapper : String -> String -> String
generateWrapper moduleName valueName =
    String.join "\n"
        [ "port module EvalWrapper__ exposing (main)"
        , ""
        , "import " ++ moduleName
        , ""
        , "port evalOutput__ : String -> Cmd msg"
        , ""
        , "main ="
        , "    Platform.worker"
        , "        { init = \\() -> ( (), evalOutput__ " ++ moduleName ++ "." ++ valueName ++ " )"
        , "        , update = \\_ m -> ( m, Cmd.none )"
        , "        , subscriptions = \\_ -> Sub.none"
        , "        }"
        , ""
        ]


stripDotSlash : String -> String
stripDotSlash s =
    if String.startsWith "./" s then
        String.dropLeft 2 s

    else
        s
