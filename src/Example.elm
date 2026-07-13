module Example exposing (run)

import Build
import BuildFile.Example
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (OptionsParser)
import Pages.Script as Script exposing (Script)
import Path exposing (Path)


run : Script
run =
    Script.withCliOptions
        (Build.programConfig
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
                        |> Option.map Path.path
                        |> Option.withDisplayName "dir"
                        |> Option.withDescription "Input directory"
                    )
            )
        )
        (Build.toTask BuildFile.Example.buildFile)
