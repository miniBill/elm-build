port module Main exposing (main, run)

import Ansi.Color as Ansi
import BackendTask
import BackendTask.Extra
import BuildTypes exposing (Block(..), Command, Engine(..))
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import ElmCodegen
import Iso8601
import Json.Decode as JD exposing (Decoder, Value)
import Pages.Script as Script exposing (Script)
import Process
import Task
import Time


port chokidarWatch : String -> Cmd msg


port log : String -> Cmd msg


port die : { message : String, exitCode : Int } -> Cmd msg


port chokidar : (Value -> msg) -> Sub msg


type alias Flags =
    List String


type Model
    = Initial
    | SettingUpChokidar { lastEvent : Time.Posix }
    | Idle


type Msg
    = WithoutTime InnerMsg
    | WithTime InnerMsg Time.Posix


type InnerMsg
    = Now
    | Chokidar Value


type alias Event =
    { path : String
    , data : EventData
    }


type EventData
    = Add { hash : String }
    | AddDir
    | Change { hash : String }
    | Unlink
    | UnlinkDir


chokidarDataDecoder : Decoder Event
chokidarDataDecoder =
    let
        withHash ctor =
            JD.map (\hash -> ctor { hash = hash }) (JD.field "hash" JD.string)
    in
    JD.map2 Event
        (JD.field "path" JD.string)
        (JD.field "eventName" JD.string
            |> JD.andThen
                (\eventName ->
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
                )
        )


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Initial
    , Cmd.batch
        [ Task.perform (WithTime Now) Time.now
        , info "Starting"
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( WithoutTime inner, _ ) ->
            ( model, Task.perform (WithTime inner) Time.now )

        ( WithTime Now now, Initial ) ->
            ( SettingUpChokidar { lastEvent = now }
            , Cmd.batch
                [ chokidarWatch "public"
                , tickAfter 30
                ]
            )

        ( WithTime (Chokidar data) now, SettingUpChokidar _ ) ->
            decodeOrDie chokidarDataDecoder data model <|
                \{ path } ->
                    ( SettingUpChokidar { lastEvent = now }, debug <| "Watching " ++ path )

        ( WithTime Now now, SettingUpChokidar { lastEvent } ) ->
            if Time.posixToMillis now - Time.posixToMillis lastEvent > 30 then
                ( Idle, info "Ready" )

            else
                ( SettingUpChokidar { lastEvent = now }, tickAfter 30 )

        ( WithTime (Chokidar data) now, Idle ) ->
            decodeOrDie chokidarDataDecoder data model <|
                \decoded ->
                    ( model
                    , debug <|
                        Debug.toString
                            { path = decoded.path
                            , data = decoded.data
                            , now = Iso8601.fromTime now
                            }
                    )

        _ ->
            ( model, die { message = "Unexpected message", exitCode = 1 } )


decodeOrDie : Decoder a -> Value -> Model -> (a -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
decodeOrDie decoder value model cont =
    case JD.decodeValue decoder value of
        Err e ->
            ( model
            , die
                { message = "Decoding failed: " ++ JD.errorToString e
                , exitCode = 1
                }
            )

        Ok v ->
            cont v


info : String -> Cmd Msg
info message =
    log (Ansi.fontColor Ansi.cyan "info  " ++ message)


debug : String -> Cmd Msg
debug message =
    log (Ansi.fontColor Ansi.brightBlack "debug " ++ message)


tickAfter : Float -> Cmd Msg
tickAfter delay =
    Task.perform (\_ -> WithoutTime Now) (Process.sleep delay)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map WithoutTime <|
        Sub.batch
            [ case model of
                Initial ->
                    Sub.none

                SettingUpChokidar _ ->
                    Sub.none

                Idle ->
                    Sub.none
            , chokidar Chokidar
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
        Pool { name, depth } ->
            blockToString "pool"
                name
                [ ( "depth"
                  , depth
                        |> String.fromInt
                        |> Just
                  )
                ]

        Rule { name, commands, pool } ->
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

        Build { rule, inputs, outputs } ->
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
