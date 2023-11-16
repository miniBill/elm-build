port module Main exposing (main, run)

import Ansi.Color as Color
import BackendTask
import BackendTask.Extra
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import ElmCodegen
import Json.Decode as JD exposing (Decoder, Value)
import Pages.Script as Script exposing (Script)
import Process
import Task
import Types exposing (Block(..), Command, Engine(..))


port die : Int -> Cmd msg


port log : String -> Cmd msg


port chokidar : (Value -> msg) -> Sub msg


type alias Flags =
    List String


type Model
    = Model
    | Dead


type Msg
    = Die Int
    | Chokidar Value


type alias ChokidarData =
    { event : Value
    , data : Value
    }


chokidarDataDecoder : Decoder ChokidarData
chokidarDataDecoder =
    JD.map2 ChokidarData
        (JD.field "event" JD.value)
        (JD.field "data" JD.value)


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model
    , Process.sleep 100
        |> Task.perform (\_ -> Die 0)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Die code ->
            ( Dead, die code )

        Chokidar data ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Model ->
            chokidar Chokidar

        Dead ->
            Sub.none


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
                |> BackendTask.Extra.log Color.cyan "Current directory" (Maybe.withDefault "unknown")
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
                        Script.log (Color.fontColor Color.brightGreen "Written build.ninja")
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
