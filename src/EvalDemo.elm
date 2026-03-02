module EvalDemo exposing (run)

{-| End-to-end demo of ElmProject.eval.

Evaluates `SampleValue.greeting` via `elm make` + `node`, with caching
based on transitive source dependencies.

-}

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import Cache
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import ElmProject
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Path exposing (Path)


run : Script
run =
    Script.withCliOptions programConfig task


type alias Config =
    { buildDirectory : Path
    }


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.requiredKeywordArg "build"
                        |> Option.map Path.path
                        |> Option.withDescription "Build folder for the eval cache"
                    )
            )


task : Config -> BackendTask FatalError ()
task config =
    Do.do (ElmProject.fromPath (Path.path ".")) <| \project ->
    Do.exec "mkdir" [ "-p", Path.toString config.buildDirectory ] <| \_ ->
    Do.do
        (Cache.run { jobs = Nothing } config.buildDirectory
            (ElmProject.eval project "SampleValue.greeting" Cache.succeed)
        )
    <| \result ->
    Do.allowFatal (File.rawFile (Path.toString result.output)) <| \content ->
    Do.do (Script.log ("Eval result: " ++ content)) <| \_ ->
    if content == "Hello from Elm eval!" then
        Script.log "SUCCESS: Output matches expected value."

    else
        BackendTask.fail
            (FatalError.fromString
                ("FAILURE: Expected \"Hello from Elm eval!\" but got: " ++ content)
            )
