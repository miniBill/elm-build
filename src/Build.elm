module Build exposing (run)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Extra
import BuildAction
import Cache
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import Set exposing (Set)


run : Script
run =
    Script.withCliOptions programConfig task


type alias Config inputs =
    { getInputs : BackendTask FatalError inputs
    , buildAction : inputs -> Cache.Monad Cache.FileOrDirectory
    , buildDirectory : Path
    , outputName : Path
    , removeStale : Bool
    }


programConfig : Program.Config (Config (List ( Path, Cache.Monad Cache.FileOrDirectory )))
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build
                (\inputDirectory ->
                    Config
                        (BuildAction.getInputs { inputDirectory = inputDirectory })
                        (BuildAction.buildAction { inputDirectory = inputDirectory })
                )
                |> OptionsParser.with
                    (Option.requiredKeywordArg "input"
                        |> Option.map Path.path
                        |> Option.withDescription "Input folder"
                    )
                |> OptionsParser.with
                    (Option.requiredKeywordArg "build"
                        |> Option.map Path.path
                        |> Option.withDescription "Build folder - contains the intermediate files"
                    )
                |> OptionsParser.with
                    (Option.requiredKeywordArg "output"
                        |> Option.map Path.path
                        |> Option.withDescription "Output folder"
                    )
                |> OptionsParser.with
                    (Option.flag "remove-stale"
                        |> Option.withDescription "Remove unused files from the build folder"
                    )
            )


task : Config inputs -> BackendTask FatalError ()
task config =
    BackendTask.Extra.profiling "main" <|
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Getting inputs") <| \_ ->
        Do.do config.getInputs <| \inputs ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Processing inputs") <| \_ ->
        Do.do (Cache.run config.buildDirectory (config.buildAction inputs)) <| \combined ->
        Do.exec "rm" [ "-f", Path.toString config.outputName ] <| \_ ->
        symlink_
            { source = config.outputName
            , target =
                Path.relativeTo
                    (Path.directory config.outputName)
                    combined.output
            }
        <| \_ ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Output: " ++ Path.toString combined.output) <| \_ ->
        Do.glob (Path.toString config.buildDirectory ++ "/*") <| \actualList ->
        if config.removeStale then
            let
                expected : Set String
                expected =
                    combined.dependencies
                        |> List.map Path.toString
                        |> Set.fromList

                actual : Set String
                actual =
                    Set.fromList actualList

                unexpected : Set String
                unexpected =
                    Set.diff actual expected
            in
            Do.log ("Removing " ++ String.fromInt (Set.size unexpected) ++ " files from the build directory") <| \_ ->
            Do.each (Set.toList unexpected) (\i -> Script.exec "chmod" [ "-R", "700", i ]) <| \_ ->
            Do.each (Set.toList unexpected) (\i -> Script.exec "rm" [ "-rf", i ]) <| \_ ->
            Do.noop

        else
            Do.noop


symlink_ : { source : Path, target : Path } -> (() -> BackendTask FatalError a) -> BackendTask FatalError a
symlink_ config k =
    Do.do (symlink config) k


symlink : { source : Path, target : Path } -> BackendTask FatalError ()
symlink { source, target } =
    Script.exec "ln" [ "-s", Path.toString target, Path.toString source ]
