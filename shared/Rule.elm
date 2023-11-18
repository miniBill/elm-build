module Rule exposing (Path, Rule, commands, convert, getMTime, getSize, list, listFiles, multiple, writeCodegenFile, writeFile)

import ConcurrentTask exposing (ConcurrentTask)
import Elm
import Internal exposing (TrackingTask(..))
import Json.Decode as JD
import Json.Encode as JE
import Set exposing (Set)
import Time
import TrackingTask


type alias Path =
    String


type alias Rule =
    { inputs : Set Path
    , outputs : Set Path
    , task : () -> ConcurrentTask String ()
    , taskDescription : List String
    }


do :
    TrackingTask a
    -> List Path
    -> (a -> List String)
    -> (a -> ConcurrentTask String ())
    -> ConcurrentTask e (List Rule)
do (TrackingTask task) outputs desc f =
    ConcurrentTask.map
        (\( inputs, a ) ->
            [ { inputs = Set.fromList inputs
              , outputs = Set.fromList outputs
              , task = \_ -> f a
              , taskDescription = desc a
              }
            ]
        )
        (ConcurrentTask.mapError never task)


listFiles : Path -> TrackingTask (List Path)
listFiles path =
    { function = "listFiles"
    , expect = ConcurrentTask.expectJson (JD.list JD.string)
    , errors = ConcurrentTask.expectNoErrors
    , args = JE.string path
    }
        |> ConcurrentTask.define
        |> ConcurrentTask.map (\v -> ( [ path ], v ))
        |> TrackingTask


{-| Runs a list of commands. `additionalInputs` should be list of files that the command will read. You don't need to specify files that were already read to create the rule, but it's fine to do so.

You should wrap this in a function that automatically keeps track of inputs and outputs.

-}
commands :
    TrackingTask a
    ->
        { outputs : List Path
        , additionalInputs : List Path
        , toCommands : a -> List (List String)
        }
    -> ConcurrentTask e (List Rule)
commands (TrackingTask task) config =
    do
        (task
            |> ConcurrentTask.map
                (\( inputs, value ) ->
                    ( config.additionalInputs ++ inputs, value )
                )
            |> TrackingTask
        )
        config.outputs
        (config.toCommands >> List.map commandToString)
        (\a ->
            List.foldl
                (\commandString acc ->
                    acc
                        |> ConcurrentTask.andThenDo
                            (rawCommand commandString
                                |> ConcurrentTask.map (\_ -> ())
                            )
                )
                (ConcurrentTask.succeed ())
                (config.toCommands a)
        )


rawCommand : List String -> ConcurrentTask String { stdout : List String, stderr : List String }
rawCommand commandString =
    { args = JE.list JE.string commandString
    , errors = ConcurrentTask.expectNoErrors
    , expect =
        ConcurrentTask.expectJson
            (JD.map3
                (\exitCode stdout stderr ->
                    { exitCode = exitCode
                    , stdout = stdout
                    , stderr = stderr
                    }
                )
                (JD.field "exitCode" JD.int)
                (JD.field "stdout" <| JD.list JD.string)
                (JD.field "stderr" <| JD.list JD.string)
            )
    , function = "command"
    }
        |> ConcurrentTask.define
        |> ConcurrentTask.andThen
            (\{ exitCode, stdout, stderr } ->
                if exitCode == 0 then
                    ConcurrentTask.succeed { stdout = stdout, stderr = stderr }

                else
                    ConcurrentTask.fail
                        ("Command "
                            ++ commandToString commandString
                            ++ " failed with exit code "
                            ++ String.fromInt exitCode
                        )
            )


commandToString : List String -> String
commandToString args =
    let
        escapeArg : String -> String
        escapeArg arg =
            let
                escaped : String
                escaped =
                    JE.encode 0 <| JE.string arg
            in
            if String.contains " " escaped || String.contains "\\" escaped then
                escaped

            else
                arg
    in
    String.join " " <| List.map escapeArg args


writeFile :
    Path
    -> TrackingTask a
    -> (a -> String)
    -> ConcurrentTask e (List Rule)
writeFile path task content =
    do task
        [ path ]
        (\_ -> [ "writing file " ++ path ])
        (\a ->
            { function = "writeFile"
            , expect = ConcurrentTask.expectWhatever
            , errors = ConcurrentTask.expectNoErrors
            , args =
                JE.object
                    [ ( "path", JE.string path )
                    , ( "content", JE.string (content a) )
                    ]
            }
                |> ConcurrentTask.define
        )


writeCodegenFile :
    TrackingTask a
    -> List String
    -> (a -> List Elm.Declaration)
    -> ConcurrentTask e (List Rule)
writeCodegenFile task moduleName declarations =
    let
        targetName : String
        targetName =
            String.join "/" ("generated" :: moduleName) ++ ".elm"
    in
    writeFile targetName
        task
        (\a -> (Elm.file moduleName (declarations a)).contents)
        |> ConcurrentTask.map
            (List.map <|
                \rule ->
                    { rule | taskDescription = [ "elm-codegen => " ++ targetName ] }
            )


getMTime : Path -> TrackingTask (Maybe Time.Posix)
getMTime path =
    stat path |> TrackingTask.map (Maybe.map <| \{ mtime } -> mtime)


getSize : Path -> TrackingTask (Maybe Int)
getSize path =
    stat path |> TrackingTask.map (Maybe.map <| \{ size } -> size)


type alias Stat =
    { size : Int
    , mtime : Time.Posix
    }


stat : String -> TrackingTask (Maybe Stat)
stat path =
    TrackingTask
        ({ function = "stat"
         , expect = ConcurrentTask.expectJson (JD.nullable statDecoder)
         , errors = ConcurrentTask.expectNoErrors
         , args = JE.string path
         }
            |> ConcurrentTask.define
            |> ConcurrentTask.map (\res -> ( [ path ], res ))
        )


statDecoder : JD.Decoder Stat
statDecoder =
    JD.map2 Stat
        (JD.field "size" JD.int)
        (JD.map (Time.millisToPosix << round) <| JD.field "mtimeMs" JD.float)


multiple : TrackingTask a -> (a -> List (ConcurrentTask e (List Rule))) -> ConcurrentTask e (List Rule)
multiple (TrackingTask task) f =
    task
        |> ConcurrentTask.mapError never
        |> ConcurrentTask.andThen
            (\( inputs, v ) ->
                let
                    inputsSet : Set String
                    inputsSet =
                        Set.fromList inputs
                in
                f v
                    |> ConcurrentTask.batch
                    |> ConcurrentTask.map
                        (\rules ->
                            rules
                                |> List.concat
                                |> List.map
                                    (\rule ->
                                        { rule | inputs = Set.union inputsSet rule.inputs }
                                    )
                        )
            )


convert : String -> String -> ConcurrentTask e (List Rule)
convert from to =
    commands (TrackingTask.succeed ())
        { outputs = [ to ]
        , additionalInputs = [ from ]
        , toCommands =
            \_ ->
                [ [ "mkdir", "-p", dirname to ]
                , [ "convert", from, to ]
                ]
        }


dirname : String -> String
dirname path =
    path
        |> String.split "/"
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> String.join "/"


list :
    TrackingTask (List a)
    -> (a -> ConcurrentTask e (List Rule))
    -> ConcurrentTask e (List Rule)
list task toRules =
    multiple task (List.map toRules)
