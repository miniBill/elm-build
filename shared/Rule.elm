module Rule exposing (Path, Rules, addInputs, batch, commands, convert, getMTime, getSize, list, listFiles, multiple, readBinary, writeCodegenFile, writeFile)

import Base64
import Bytes exposing (Bytes)
import ConcurrentTask exposing (ConcurrentTask)
import Elm
import Internal exposing (TrackingTask(..))
import Json.Decode as JD
import Json.Encode as JE
import Set exposing (Set)
import Time
import TrackingTask exposing (TrackingTask)


type alias Path =
    String


type alias Rules =
    Internal.Rules


do :
    TrackingTask a
    -> List Path
    -> (a -> List String)
    -> (a -> ConcurrentTask String ())
    -> Rules
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
        task
        |> Internal.Rules


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
    -> Rules
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


readBinary : String -> TrackingTask Bytes
readBinary path =
    { function = "readBinary"
    , expect = ConcurrentTask.expectString
    , errors = ConcurrentTask.expectNoErrors
    , args = JE.string path
    }
        |> ConcurrentTask.define
        |> ConcurrentTask.andThen
            (\base64 ->
                case Base64.toBytes base64 of
                    Nothing ->
                        ConcurrentTask.fail "Unable to decode base64 content from JS"

                    Just bytes ->
                        ConcurrentTask.succeed ( [ path ], bytes )
            )
        |> TrackingTask


writeFile :
    Path
    -> TrackingTask a
    -> (a -> String)
    -> Rules
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
    -> Rules
writeCodegenFile task moduleName declarations =
    let
        targetName : String
        targetName =
            String.join "/" ("generated" :: moduleName) ++ ".elm"

        (Internal.Rules inner) =
            writeFile targetName
                task
                (\a -> (Elm.file moduleName (declarations a)).contents)
    in
    inner
        |> ConcurrentTask.map
            (List.map <|
                \rule ->
                    { rule | taskDescription = [ "elm-codegen => " ++ targetName ] }
            )
        |> Internal.Rules


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


multiple : TrackingTask a -> (a -> List Rules) -> Rules
multiple (TrackingTask task) f =
    task
        |> ConcurrentTask.andThen
            (\( inputs, v ) ->
                let
                    inputsSet : Set String
                    inputsSet =
                        Set.fromList inputs

                    (Internal.Rules inner) =
                        f v
                            |> batch
                            |> addInputs inputsSet
                in
                inner
            )
        |> Internal.Rules


addInputs : Set String -> Rules -> Rules
addInputs additionalInputs (Internal.Rules task) =
    Internal.Rules
        (ConcurrentTask.map
            (List.map
                (\rule ->
                    { rule
                        | inputs = Set.union additionalInputs rule.inputs
                    }
                )
            )
            task
        )


convert : String -> String -> Rules
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
    -> (a -> Rules)
    -> Rules
list task toRules =
    multiple task (List.map toRules)


batch : List Rules -> Rules
batch rules =
    rules
        |> List.map (\(Internal.Rules rule) -> rule)
        |> ConcurrentTask.batch
        |> ConcurrentTask.map List.concat
        |> Internal.Rules
