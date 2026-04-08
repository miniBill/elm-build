module Build exposing (Config, HashKind, fastHash, run, secureHash, toTask)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Customs
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.Time
import BuildTask exposing (BuildTask, FileOrDirectory)
import Buildfile
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError exposing (FatalError)
import Hash
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import Set exposing (Set)
import Time


run : Script
run =
    Script.withCliOptions programConfig toTask


type alias HashKind =
    Hash.Kind


type alias Config inputs =
    { getInputs : BackendTask FatalError inputs
    , buildAction : inputs -> BuildTask FileOrDirectory
    , buildDirectory : Path
    , outputName : Path
    , removeStale : Bool
    , jobs : Maybe Int
    , debug : Bool
    , hashKind : HashKind
    }


{-| Use FNV1a for hashing. Suitable for local development.
-}
fastHash : HashKind
fastHash =
    Hash.Fast


{-| Use SHA256 for hashing. Suitable for a CI environment.
-}
secureHash : HashKind
secureHash =
    Hash.Secure


programConfig : Program.Config (Config (List ( Path, BuildTask FileOrDirectory )))
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build
                (\inputDirectory ->
                    Config
                        (Buildfile.getInputs { inputDirectory = inputDirectory })
                        (Buildfile.buildAction { inputDirectory = inputDirectory })
                )
                |> OptionsParser.with
                    (Option.requiredKeywordArg "input"
                        |> Option.map Path.path
                        |> Option.withDisplayName "dir"
                        |> Option.withDescription "Input directory"
                    )
                |> OptionsParser.with
                    (Option.requiredKeywordArg "build"
                        |> Option.map Path.path
                        |> Option.withDisplayName "dir"
                        |> Option.withDescription "Build directory - contains the intermediate files"
                    )
                |> OptionsParser.with
                    (Option.requiredKeywordArg "output"
                        |> Option.map Path.path
                        |> Option.withDisplayName "dir"
                        |> Option.withDescription "Output directory"
                    )
                |> OptionsParser.with
                    (Option.flag "remove-stale"
                        |> Option.withDescription "Remove unused files from the build directory"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "jobs"
                        |> Option.withDescription "Number of parallel jobs to run"
                        |> Option.withDisplayName "n"
                        |> Option.validateMapIfPresent
                            (\j ->
                                case String.toInt j of
                                    Nothing ->
                                        Err ("Invalid number of jobs: " ++ j)

                                    Just i ->
                                        Ok i
                            )
                    )
                |> OptionsParser.with
                    (Option.flag "debug"
                        |> Option.withDescription "Output debug info"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "hash-kind"
                        |> Option.withDescription "Kind of hash to use. Choose fast for FNV1a, secure for sha256."
                        |> Option.withDefault "fast"
                        |> Option.oneOf [ ( "fast", Hash.Fast ), ( "secure", Hash.Secure ) ]
                    )
            )


toTask : Config inputs -> BackendTask FatalError ()
toTask config =
    BackendTask.Extra.profiling "main" <|
        Do.do BackendTask.Time.now <| \begin ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Getting inputs") <| \_ ->
        Do.do config.getInputs <| \inputs ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Processing inputs") <| \_ ->
        Do.exec "mkdir" [ "-p", Path.toString config.buildDirectory ] <| \_ ->
        Do.do (BuildTask.run { jobs = config.jobs, debug = config.debug, hashKind = config.hashKind } config.buildDirectory (config.buildAction inputs)) <| \combined ->
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
        Do.do (BackendTask.Customs.readdir config.buildDirectory) <| \actualList ->
        let
            expected : Set String
            expected =
                combined.intermediate
                    |> List.map Path.toString
                    |> Set.fromList

            actual : Set String
            actual =
                actualList
                    |> List.map (\file -> Path.toString config.buildDirectory ++ "/" ++ file)
                    |> Set.fromList

            unexpected : Set String
            unexpected =
                Set.diff actual expected
        in
        if config.removeStale then
            Do.log ("Removing " ++ String.fromInt (Set.size unexpected) ++ " files from the build directory") <| \_ ->
            Do.do
                (Set.toList unexpected
                    |> List.map
                        (\i ->
                            Do.exec "chmod" [ "-R", "700", i ] <| \_ ->
                            Script.exec "rm" [ "-rf", i ]
                        )
                    |> BackendTask.Extra.sequence_
                )
            <| \_ ->
            Do.do BackendTask.Time.now <| \end ->
            let
                elapsed : Int
                elapsed =
                    Time.posixToMillis end - Time.posixToMillis begin
            in
            Script.log ("Build done in " ++ timeToString elapsed)

        else
            Script.log (String.fromInt (Set.size unexpected) ++ " stale files in the build directory")


timeToString : Int -> String
timeToString ms =
    let
        s : Int
        s =
            ms // 1000

        m : Int
        m =
            s // 60
    in
    if m > 0 then
        String.fromInt m ++ "m " ++ String.fromInt (modBy 60 s) ++ "s " ++ String.fromInt (modBy 1000 ms) ++ "ms"

    else if s > 0 then
        String.fromFloat (toFloat ms / 1000) ++ "s"

    else
        String.fromInt ms ++ "ms"


symlink_ : { source : Path, target : Path } -> (() -> BackendTask FatalError a) -> BackendTask FatalError a
symlink_ config k =
    Do.do (symlink config) k


symlink : { source : Path, target : Path } -> BackendTask FatalError ()
symlink { source, target } =
    Script.exec "ln" [ "-s", Path.toString target, Path.toString source ]
