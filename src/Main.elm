module Main exposing (run)

import Ansi.Color as Color
import BackendTask
import BackendTask.Extra
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import ElmCodegen
import Pages.Script as Script exposing (Script)
import Types exposing (Block(..), Command, Engine(..))


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
        |> (\l -> l ++ [ "default generated/Fonts.elm" ])
        |> String.join "\n\n"


toNinjaBlock : Block -> String
toNinjaBlock block =
    case block of
        Pool { name, depth } ->
            "pool " ++ name ++ "\n  depth = " ++ String.fromInt depth

        Rule { name, commands, pool } ->
            let
                commandToString : Command -> String
                commandToString command =
                    String.join " " (List.map escape command)
            in
            [ Just <| "rule " ++ name
            , Just <| "command = " ++ String.join " && " (List.map commandToString commands)
            , Maybe.map (\poolName -> "pool = " ++ poolName) pool
            ]
                |> List.filterMap identity
                |> String.join "\n  "

        Build { rule, inputs, outputs } ->
            "build "
                ++ String.join " " (List.map escape outputs)
                ++ ": "
                ++ rule
                ++ " "
                ++ String.join " " (List.map escape inputs)


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
