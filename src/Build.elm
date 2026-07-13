module Build exposing (BuildFile, Config, HashKind, customProgramConfig, programConfig, toTask)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Customs
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.Time
import BuildTask exposing (BuildTask, FileOrDirectory)
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (OptionsParser)
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.Program as Program
import FastSet as Set exposing (Set)
import FatalError exposing (FatalError)
import Hash
import Pages.Script as Script
import Path exposing (Path)
import Time


type HashKind
    = HashKind Hash.Kind


type alias Config custom =
    { custom
        | buildPath : Path
        , outputName : Path
        , jobs : Maybe Int
        , removeStale : Bool
        , check : Bool
        , keepFailed : Bool
        , hashKind : HashKind
        , debug : Bool
    }


{-| -}
type alias BuildFile customConfig inputs tools =
    { getInputs : Config customConfig -> BackendTask FatalError inputs
    , getTools : Config customConfig -> inputs -> BuildTask () FatalError tools
    , buildAction : Config customConfig -> inputs -> BuildTask tools FatalError FileOrDirectory
    }


{-| Use FNV1a for hashing. Suitable for local development.
-}
fastHash : HashKind
fastHash =
    HashKind Hash.Fast


{-| Use SHA256 for hashing. Suitable for a CI environment.
-}
secureHash : HashKind
secureHash =
    HashKind Hash.Secure


programConfig : Program.Config (Config {})
programConfig =
    customProgramConfig (OptionsParser.build identity)


customProgramConfig : OptionsParser (Config {} -> Config customConfig) BuilderState.AnyOptions -> Program.Config (Config customConfig)
customProgramConfig optionsParser =
    Program.config
        |> Program.add
            (optionsParser
                |> OptionsParser.map
                    (\f buildPath outputName jobs removeStale check keepFailed hashKind debug ->
                        f
                            { buildPath = buildPath
                            , outputName = outputName
                            , jobs = jobs
                            , removeStale = removeStale
                            , check = check
                            , keepFailed = keepFailed
                            , hashKind = hashKind
                            , debug = debug
                            }
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
                    (Option.flag "remove-stale"
                        |> Option.withDescription "Remove unused files from the build directory"
                    )
                |> OptionsParser.with
                    (Option.flag "check"
                        |> Option.withDescription "Re-run all commands to check for determinism"
                    )
                |> OptionsParser.with
                    (Option.flag "keep-failed"
                        |> Option.withDescription "Keep intermediate folder of failed commands"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "hash-kind"
                        |> Option.withDescription "Kind of hash to use. Choose fast for FNV1a, secure for sha256."
                        |> Option.withDefault "fast"
                        |> Option.oneOf [ ( "fast", fastHash ), ( "secure", secureHash ) ]
                    )
                |> OptionsParser.with
                    (Option.flag "debug"
                        |> Option.withDescription "Output debug info"
                    )
            )


toTask : BuildFile customConfig inputs tools -> Config customConfig -> BackendTask FatalError ()
toTask buildFile config =
    BackendTask.Extra.profiling "main" <|
        Do.do BackendTask.Time.now <| \begin ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Getting inputs") <| \_ ->
        Do.do (buildFile.getInputs config) <| \inputs ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Processing inputs") <| \_ ->
        Do.exec "mkdir" [ "-p", Path.toString config.buildPath ] <| \_ ->
        Do.do
            (BuildTask.run
                { jobs = config.jobs
                , debug = config.debug
                , check = config.check
                , hashKind =
                    let
                        (HashKind kind) =
                            config.hashKind
                    in
                    kind
                , keepFailed = config.keepFailed
                , getTools = buildFile.getTools config inputs
                }
                config.buildPath
                (buildFile.buildAction config inputs)
                |> BackendTask.mapError buildErrorToFatalError
            )
        <| \combined ->
        Do.do
            (Script.removeFile (Path.toString config.outputName)
                |> BackendTask.onError (\_ -> BackendTask.succeed ())
            )
        <| \_ ->
        symlink
            { source = config.outputName
            , target =
                Path.relativeTo
                    (Path.directory config.outputName)
                    combined.output
            }
        <| \_ ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Output: " ++ Path.toString combined.output) <| \_ ->
        Do.do (BackendTask.Customs.readdir config.buildPath) <| \actualList ->
        let
            expected : Set String
            expected =
                combined.intermediate
                    |> List.map Path.toString
                    |> Set.fromList

            actual : Set String
            actual =
                actualList
                    |> List.map (\file -> Path.toString config.buildPath ++ "/" ++ file)
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

                msg : String
                msg =
                    "Build done in "
                        ++ timeToString elapsed
                        ++ " with "
                        ++ String.fromInt (Set.size combined.warnings)
                        ++ " "
                        ++ plural (Set.size combined.warnings) "warning" "warnings"
            in
            Script.log msg

        else
            Script.log (String.fromInt (Set.size unexpected) ++ " stale files in the build directory")


buildErrorToFatalError : BuildTask.Error FatalError -> FatalError
buildErrorToFatalError e =
    case e of
        BuildTask.InternalError i ->
            i

        BuildTask.UserError u ->
            u


plural : Int -> String -> String -> String
plural n singular plural_ =
    if n == 1 then
        singular

    else
        plural_


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


symlink : { source : Path, target : Path } -> (() -> BackendTask FatalError a) -> BackendTask FatalError a
symlink { source, target } k =
    Do.do (Script.exec "ln" [ "-s", Path.toString target, Path.toString source ]) k
