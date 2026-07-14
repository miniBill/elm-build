module Main exposing (run)

import Build
import Example
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (OptionsParser)
import Pages.Script as Script exposing (Script)
import Path.Posix as Path exposing (Path)


run : Script
run =
    Script.withCliOptions
        (Build.customProgramConfig
            (OptionsParser.build
                (\inputPath config ->
                    { inputPath = inputPath
                    , buildPath = config.buildPath
                    , outputName = config.outputName
                    , jobs = config.jobs
                    , removeStale = config.removeStale
                    , check = config.check
                    , keepFailed = config.keepFailed
                    , hashKind = config.hashKind
                    , debug = config.debug
                    }
                )
                |> OptionsParser.with
                    (Option.requiredKeywordArg "input"
                        |> Option.withDisplayName "dir"
                        |> Option.withDescription "Input directory"
                    )
            )
        )
        (Build.toTask Example.buildFile)
