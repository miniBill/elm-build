port module Main exposing (main, run)

import Ansi.Color as Ansi
import BackendTask
import BackendTask.Extra
import BuildTypes exposing (Block(..), Command, Engine(..))
import Buildfile
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Process
import ConcurrentTask.Time
import Dict exposing (Dict)
import ElmCodegen
import Iso8601
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import Pages.Script as Script exposing (Script)
import Process
import Rule exposing (Path, Rule, TrackingTask)
import Task
import Time


port send : Value -> Cmd msg


port receive : (Value -> msg) -> Sub msg


port chokidar : (Value -> msg) -> Sub msg


type alias Flags =
    List String


type alias Options =
    { digest : Bool
    }


type alias Model =
    { pool : ConcurrentTask.Pool Msg Never Msg
    , inner : InnerModel
    }


type InnerModel
    = Initial Options
    | Building Options
    | SettingUpChokidar Options { lastEvent : Time.Posix }
    | Idle Options { lastEvent : Time.Posix }
    | PreparingBuild Options


type Msg
    = WithoutTime InnerMsg
    | WithTime InnerMsg Time.Posix
    | OnProgress ( ConcurrentTask.Pool Msg Never Msg, Cmd Msg )
    | OnComplete (ConcurrentTask.Response Never Msg)
    | GotRules (List Rule)


type InnerMsg
    = Build Options
    | Chokidar Value
    | DoneBuilding
    | SetupChokidar (List Path)
    | NoOp


type alias Event =
    { path : String
    , mtime : Time.Posix
    , data : EventData
    }


eventDecoder : Decoder Event
eventDecoder =
    JD.map3 Event
        (JD.field "path" JD.string)
        (JD.at [ "stats", "mtimeMs" ] (JD.map (Time.millisToPosix << round) JD.float))
        eventDataDecoder


type EventData
    = Add { hash : Maybe String }
    | AddDir
    | Change { hash : Maybe String }
    | Unlink
    | UnlinkDir


eventDataDecoder : Decoder EventData
eventDataDecoder =
    let
        withHash : ({ hash : Maybe String } -> EventData) -> Decoder EventData
        withHash ctor =
            JD.map (\hash -> ctor { hash = hash }) (JD.maybe <| JD.field "hash" JD.string)

        inner : String -> Decoder EventData
        inner eventName =
            case eventName of
                "add" ->
                    withHash Add

                "change" ->
                    withHash Change

                "unlink" ->
                    JD.succeed Unlink

                "addDir" ->
                    JD.succeed AddDir

                "unlinkDir" ->
                    JD.succeed UnlinkDir

                _ ->
                    JD.fail <| "Unexpected event name: " ++ eventName
    in
    JD.field "eventName" JD.string
        |> JD.andThen inner


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        options : Options
        options =
            { digest = List.member "--digest" flags
            }
    in
    ( { pool = ConcurrentTask.pool
      , inner = Initial options
      }
    , Task.perform (\_ -> WithoutTime (Build options)) (Process.sleep 0)
    )


attempt : Model -> ConcurrentTask Never Msg -> ( Model, Cmd Msg )
attempt model task =
    let
        ( newPool, cmd ) =
            ConcurrentTask.attempt
                { send = send
                , pool = model.pool
                , onComplete = OnComplete
                }
                task
    in
    ( { model | pool = newPool }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.inner ) of
        ( WithoutTime inner, _ ) ->
            ( model, Task.perform (WithTime inner) Time.now )

        ( WithTime (Build options) now, _ ) ->
            info now "Building..."
                |> ConcurrentTask.andThenDo (Rule.getRules Buildfile.build)
                |> ConcurrentTask.andThen
                    (\rules ->
                        build rules
                            |> ConcurrentTask.map
                                (\_ -> WithoutTime <| SetupChokidar (List.concatMap .inputs rules))
                    )
                |> attempt { model | inner = Building options }

        ( WithTime (SetupChokidar targets) now, Building options ) ->
            info now "Build done, setting up chokidar..."
                |> ConcurrentTask.andThenDo (setupChokidar options targets)
                |> attempt { model | inner = SettingUpChokidar options { lastEvent = now } }

        ( WithTime (Chokidar data) now, SettingUpChokidar options _ ) ->
            (decodeOrDie eventDecoder data <|
                \{ path } ->
                    debug now ("Watching " ++ path)
                        |> ConcurrentTask.andThenDo (tickAfter 30)
            )
                |> attempt
                    { model
                        | inner =
                            SettingUpChokidar options { lastEvent = now }
                    }

        ( WithTime NoOp now, SettingUpChokidar options { lastEvent } ) ->
            if Time.posixToMillis now - Time.posixToMillis lastEvent >= 30 then
                info now "Ready, preparing build"
                    |> ConcurrentTask.andThenDo (Rule.getRules Buildfile.build)
                    |> ConcurrentTask.map GotRules
                    |> attempt { model | inner = PreparingBuild options }

            else
                ( model, Cmd.none )

        ( WithTime (Chokidar data) now, Idle options _ ) ->
            (decodeOrDie eventDecoder data <|
                \decoded ->
                    debug now
                        (Ansi.fontColor Ansi.green "[Chokidar] "
                            ++ decoded.path
                            ++ " "
                            ++ Debug.toString decoded.data
                        )
                        |> ConcurrentTask.andThenDo (tickAfter 30)
            )
                |> attempt { model | inner = Idle options { lastEvent = now } }

        ( WithTime NoOp _, _ ) ->
            ( model, Cmd.none )

        _ ->
            die { message = "Unexpected (msg, model) pair " ++ Debug.toString ( msg, model ), exitCode = 1 }
                |> attempt model


setupChokidar : Options -> List Path -> ConcurrentTask Never Msg
setupChokidar arg1 arg2 =
    Debug.todo "TODO"


build : List Rule -> ConcurrentTask e Bool
build arg1 =
    Debug.todo "TODO"


decodeOrDie : Decoder a -> Value -> (a -> ConcurrentTask e Msg) -> ConcurrentTask e Msg
decodeOrDie decoder value cont =
    case JD.decodeValue decoder value of
        Err e ->
            die
                { message = "Decoding failed: " ++ JD.errorToString e
                , exitCode = 1
                }

        Ok v ->
            cont v


die : { message : String, exitCode : Int } -> ConcurrentTask e Msg
die { message, exitCode } =
    { function = "die"
    , expect = ConcurrentTask.expectWhatever
    , errors = ConcurrentTask.expectNoErrors
    , args = JE.object [ ( "message", JE.string message ), ( "exitCode", JE.int exitCode ) ]
    }
        |> ConcurrentTask.define
        |> ConcurrentTask.map (\_ -> WithoutTime NoOp)


info : Time.Posix -> String -> ConcurrentTask e Msg
info now message =
    message
        |> timedLog now Ansi.cyan "info"


debug : Time.Posix -> String -> ConcurrentTask e Msg
debug now message =
    message
        |> timedLog now Ansi.brightBlack "debug"


timedLog : Time.Posix -> Ansi.Color -> String -> String -> ConcurrentTask e Msg
timedLog now color class message =
    ConcurrentTask.define
        { function = "log"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectNoErrors
        , args =
            JE.string <|
                ("["
                    ++ String.slice 11 -5 (Iso8601.fromTime now)
                    ++ "] "
                )
                    ++ Ansi.fontColor color (String.padRight 6 ' ' class)
                    ++ message
        }
        |> ConcurrentTask.map (\_ -> WithoutTime NoOp)


tickAfter : Int -> ConcurrentTask e Msg
tickAfter delay =
    ConcurrentTask.Process.sleep delay
        |> ConcurrentTask.map (\_ -> WithoutTime NoOp)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map WithoutTime <| chokidar Chokidar
        , ConcurrentTask.onProgress
            { send = send
            , receive = receive
            , onProgress = OnProgress
            }
            model.pool
        ]


config :
    { targets : List String
    , engines : List Engine
    }
config =
    { targets = elmCodegenOutput
    , engines =
        [ ElmCodegen.engine
            { flagsFrom = Just "public"
            , outputs = elmCodegenOutput
            }
        ]
    }


elmCodegenOutput : List String
elmCodegenOutput =
    [ "generated/Types/UserIdDict.elm"
    , "generated/Types/GameIdDict.elm"
    , "generated/Types/TokenDict.elm"
    , "generated/Images.elm"
    , "generated/Fonts.elm"
    ]


run : Script
run =
    Script.withCliOptions program
        (\_ ->
            BackendTask.Extra.pwd
                |> BackendTask.Extra.log Ansi.cyan "Current directory" (Maybe.withDefault "unknown")
                |> BackendTask.andThen
                    (\_ ->
                        BackendTask.Extra.combineMap
                            (\(Engine bt) -> bt)
                            config.engines
                    )
                |> BackendTask.map List.concat
                |> BackendTask.andThen
                    (\engines ->
                        Script.writeFile
                            { path = "build.ninja"
                            , body = toNinjafile engines ++ "\n"
                            }
                            |> BackendTask.allowFatal
                    )
                |> BackendTask.andThen
                    (\_ ->
                        Script.log (Ansi.fontColor Ansi.brightGreen "Written build.ninja")
                    )
        )


toNinjafile : List Block -> String
toNinjafile rules =
    rules
        |> List.map toNinjaBlock
        |> String.join "\n\n"


toNinjaBlock : Block -> String
toNinjaBlock block =
    let
        blockToString : String -> String -> List ( String, Maybe String ) -> String
        blockToString kind name lines =
            ((kind ++ " " ++ name)
                :: List.filterMap
                    (\( key, value ) ->
                        Maybe.map (\v -> key ++ " = " ++ v) value
                    )
                    lines
            )
                |> String.join "\n  "

        commandToString : Command -> String
        commandToString command =
            String.join " " (List.map escape command)
    in
    case block of
        BuildTypes.Pool { name, depth } ->
            blockToString "pool"
                name
                [ ( "depth"
                  , depth
                        |> String.fromInt
                        |> Just
                  )
                ]

        BuildTypes.Rule { name, commands, pool } ->
            blockToString "rule"
                name
                [ ( "command"
                  , commands
                        |> List.map commandToString
                        |> String.join " && "
                        |> Just
                  )
                , ( "pool", pool )
                ]

        BuildTypes.Build { rule, inputs, outputs } ->
            let
                line : List String
                line =
                    List.map escape outputs
                        ++ [ ":", rule ]
                        ++ List.map escape inputs
            in
            blockToString "build"
                (String.join " " line)
                []


escape : String -> String
escape arg =
    if String.contains " " arg || String.contains "'" arg then
        "'" ++ String.replace "'" "\\'" (String.replace "\\" "\\\\" arg) ++ "'"

    else
        arg


type alias CliOptions =
    {}


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions)
