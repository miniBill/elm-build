module BuildTask.Internal exposing (BuildTask(..), Command, DownloadError(..), Error(..), Input, State, Warning, allowFatal, andThen, andThen2, combineBy, commandLog, commandLogWith, deriveDirectory, deriveFile, downloadSHA256, execLog, execUnlogged, extendHashWith, extractFromDirectory, fail, fatalToInternal, hashFromString, inputFile, jobs, map, map2, map3, map4, map5, mapError, named, run, sequence, succeed, timed, toResult, triggerDebugger, which, withDebug, withEnv, withFile, withIdlePriority, withMemoryLimitInBytes, withPrefix, withWarning)

import BackendTask exposing (BackendTask)
import BackendTask.Customs
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.File.Extra
import BackendTask.Http as Http
import BackendTask.Stream as Stream exposing (Stream)
import BuildTask.CommandOptions exposing (CommandOptions)
import Duration
import FastDict as Dict exposing (Dict)
import FastSet as Set exposing (Set)
import FatalError exposing (FatalError)
import Hash exposing (Hash, Normal, Temporary)
import HashSet exposing (HashSet)
import List.Extra
import Pages.Script as Script
import Path.Posix as Path exposing (Path)
import Utils
import XBytes


type BuildTask e a
    = BuildTask String (Input -> State -> BackendTask (Error e) ( a, State ))


type Error e
    = InternalError FatalError
    | UserError e


type alias State =
    { deps : HashSet
    , warnings : Set String
    }


type alias Command =
    { name : String
    , hash : Hash Normal
    }


type alias Warning =
    String


type alias Input =
    { existing : HashSet
    , prefix : List String
    , buildPath : Path Path.Absolute Path.Directory
    , jobs : Int
    , debug : Bool
    , check : Bool
    , keepFailed : Bool
    , idlePriority : Bool
    , hashKind : Hash.Kind
    , env : Dict String String
    , memoryLimit : Maybe Int
    }


deriveDirectory :
    String
    -> Hash Normal
    -> (Input -> Path Path.Absolute Path.Directory -> BackendTask (Error e) ())
    -> BuildTask e (Hash Normal)
deriveDirectory description target inner =
    derive
        { toTemporary = Hash.toTemporaryDirectory
        , toPath = Hash.toDirectoryPath
        , name = Path.dirname
        }
        description
        target
        inner


deriveFile :
    String
    -> Hash Normal
    -> (Input -> Path Path.Absolute Path.File -> BackendTask (Error e) ())
    -> BuildTask e (Hash Normal)
deriveFile description target inner =
    derive
        { toTemporary = Hash.toTemporaryFile
        , toPath = Hash.toFilePath
        , name = Path.filename
        }
        description
        target
        inner


{-| Core primitive.

**CORRECTNESS:**

  - The task must be deterministic,
  - the hash must contain all the information the task depends on.

-}
derive :
    { toTemporary : Path Path.Absolute Path.Directory -> Hash Temporary -> Path Path.Absolute fileOrDirectory
    , toPath : Path Path.Absolute Path.Directory -> Hash Normal -> Path Path.Absolute fileOrDirectory
    , name : Path Path.Absolute fileOrDirectory -> Path Path.Relative fileOrDirectory
    }
    -> String
    -> Hash Normal
    -> (Input -> Path Path.Absolute fileOrDirectory -> BackendTask (Error e) ())
    -> BuildTask e (Hash Normal)
derive path description target inner =
    BuildTask "derive"
        (\({ existing, buildPath } as input) state ->
            let
                newDeps : HashSet
                newDeps =
                    HashSet.insert target state.deps

                appendLog : BackendTask (Error e) ()
                appendLog =
                    if input.debug then
                        (Path.toString (Hash.toFilePath buildPath target)
                            ++ ": "
                            ++ description
                         -- ++ " from "
                         -- ++ (deps
                         --         |> HashSet.toList
                         --         |> List.map (Hash.hashToPath buildPath)
                         --         |> String.join ", "
                         --    )
                        )
                            |> Script.log

                    else
                        BackendTask.succeed ()
            in
            Do.do appendLog <| \_ ->
            if (HashSet.member target existing && not input.check) || HashSet.member target state.deps then
                BackendTask.succeed ( target, { deps = newDeps, warnings = state.warnings } )

            else
                let
                    targetPath : Path Path.Absolute fileOrDirectory
                    targetPath =
                        path.toPath buildPath target
                in
                Do.do (File.exists (Path.toString targetPath)) <| \exists ->
                if exists && not input.check then
                    BackendTask.succeed ( target, { deps = newDeps, warnings = state.warnings } )

                else
                    let
                        tmp : Hash Temporary
                        tmp =
                            Hash.toTemporary target

                        tmpPath : Path Path.Absolute fileOrDirectory
                        tmpPath =
                            path.toTemporary buildPath tmp
                    in
                    Do.do
                        (BackendTask.File.Extra.removeIfExists tmpPath
                            |> BackendTask.mapError InternalError
                        )
                    <| \_ ->
                    Do.do
                        (inner input tmpPath
                            |> BackendTask.onError
                                (\e ->
                                    if input.keepFailed then
                                        case Path.parseRelativeDirectory "failed" of
                                            Nothing ->
                                                FatalError.fromString "Failed to build failed path"
                                                    |> InternalError
                                                    |> BackendTask.fail

                                            Just failed ->
                                                Do.do
                                                    (BackendTask.File.Extra.move
                                                        { from = tmpPath
                                                        , to =
                                                            Path.append buildPath
                                                                (Path.append
                                                                    failed
                                                                    (path.name tmpPath)
                                                                )
                                                        }
                                                        |> BackendTask.mapError InternalError
                                                    )
                                                <| \_ ->
                                                BackendTask.fail e

                                    else
                                        Do.do
                                            (BackendTask.File.Extra.removeIfExists tmpPath
                                                |> BackendTask.mapError InternalError
                                            )
                                        <| \_ ->
                                        BackendTask.fail e
                                )
                        )
                    <| \_ ->
                    Do.do
                        (if exists then
                            Do.do
                                (execUnlogged input "diff" [ "-r", Path.toString tmpPath, Path.toString targetPath ]
                                    |> BackendTask.onError
                                        (\e ->
                                            Do.log ("Error inside " ++ description) <| \_ ->
                                            BackendTask.fail (InternalError e)
                                        )
                                )
                            <| \_ ->
                            BackendTask.File.Extra.removeIfExists tmpPath
                                |> BackendTask.mapError InternalError

                         else
                            BackendTask.File.Extra.move { from = tmpPath, to = targetPath }
                                |> BackendTask.onError
                                    (\e ->
                                        Do.log ("Error inside " ++ description) <| \_ ->
                                        BackendTask.fail (InternalError e)
                                    )
                        )
                    <| \_ ->
                    Do.do
                        (execUnlogged input "chmod" [ "-R", "a=rX", Path.toString targetPath ]
                            |> BackendTask.mapError InternalError
                        )
                    <| \_ ->
                    BackendTask.succeed ( target, { deps = newDeps, warnings = state.warnings } )
        )


runMonad : BuildTask e a -> Input -> State -> BackendTask (Error e) ( a, State )
runMonad (BuildTask label m) input state =
    if input.debug then
        Dict.foldl BackendTask.withEnv (m input state) input.env
            |> BackendTask.andThen
                (\( v, newState ) ->
                    if HashSet.equals (HashSet.union state.deps newState.deps) newState.deps then
                        if Set.equals (Set.union state.warnings newState.warnings) newState.warnings then
                            BackendTask.succeed ( v, newState )

                        else
                            ("Missed warning inside " ++ label)
                                |> FatalError.fromString
                                |> InternalError
                                |> BackendTask.fail

                    else
                        ("Missed dependency inside " ++ label)
                            |> FatalError.fromString
                            |> InternalError
                            |> BackendTask.fail
                )

    else
        Dict.foldl BackendTask.withEnv (m input state) input.env


{-| -}
map : (a -> b) -> BuildTask e a -> BuildTask e b
map f m =
    BuildTask "map"
        (\input deps ->
            runMonad m input deps
                |> BackendTask.map
                    (\( v, output ) ->
                        ( f v, output )
                    )
        )


{-| -}
map2 :
    (a -> b -> c)
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
map2 f a b =
    BuildTask "map2"
        (\input deps ->
            BackendTask.map2
                (\( va, outputA ) ( vb, outputB ) ->
                    ( f va vb, combineOutput outputA outputB )
                )
                (runMonad a input deps)
                (runMonad b input deps)
        )


combineOutput : State -> State -> State
combineOutput l r =
    { deps = HashSet.union l.deps r.deps
    , warnings = Set.union l.warnings r.warnings
    }


combineOutputs : List State -> State
combineOutputs ls =
    let
        ( deps, warnings ) =
            List.foldl (\l ( d, w ) -> ( l.deps :: d, l.warnings :: w )) ( [], [] ) ls
    in
    { deps = HashSet.unionAll deps
    , warnings = List.foldl Set.union Set.empty warnings
    }


{-| -}
map3 :
    (a -> b -> c -> d)
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
    -> BuildTask e d
map3 f a b c =
    BuildTask "map3"
        (\input deps ->
            BackendTask.map3
                (\( va, outputA ) ( vb, outputB ) ( vc, outputC ) ->
                    ( f va vb vc, combineOutputs [ outputA, outputB, outputC ] )
                )
                (runMonad a input deps)
                (runMonad b input deps)
                (runMonad c input deps)
        )


{-| -}
map4 :
    (a -> b -> c -> d -> r)
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
    -> BuildTask e d
    -> BuildTask e r
map4 f a b c d =
    BuildTask "map4"
        (\input deps ->
            BackendTask.map4
                (\( va, outputA ) ( vb, outputB ) ( vc, outputC ) ( vd, outputD ) ->
                    ( f va vb vc vd
                    , combineOutputs [ outputA, outputB, outputC, outputD ]
                    )
                )
                (runMonad a input deps)
                (runMonad b input deps)
                (runMonad c input deps)
                (runMonad d input deps)
        )


{-| -}
map5 :
    (a -> b -> c -> d -> f -> g)
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
    -> BuildTask e d
    -> BuildTask e f
    -> BuildTask e g
map5 f a b c d e =
    BuildTask "map5"
        (\input deps ->
            BackendTask.map5
                (\( va, outputA ) ( vb, outputB ) ( vc, outputC ) ( vd, outputD ) ( ve, outputE ) ->
                    ( f va vb vc vd ve
                    , combineOutputs [ outputA, outputB, outputC, outputD, outputE ]
                    )
                )
                (runMonad a input deps)
                (runMonad b input deps)
                (runMonad c input deps)
                (runMonad d input deps)
                (runMonad e input deps)
        )


{-| -}
andThen : (a -> BuildTask e b) -> BuildTask e a -> BuildTask e b
andThen f m =
    BuildTask "andThen"
        (\input deps ->
            runMonad m input deps
                |> BackendTask.andThen
                    (\( v, newDeps ) ->
                        runMonad (f v) input newDeps
                    )
        )


{-| -}
andThen2 : (a -> b -> BuildTask e c) -> BuildTask e a -> BuildTask e b -> BuildTask e c
andThen2 f l r =
    BuildTask "andThen2"
        (\input deps ->
            BackendTask.map2
                Tuple.pair
                (runMonad l input deps)
                (runMonad r input deps)
                |> BackendTask.andThen
                    (\( ( lv, lOutput ), ( rv, rOutput ) ) ->
                        runMonad (f lv rv) input (combineOutput lOutput rOutput)
                    )
        )


withFile :
    Hash Normal
    -> (String -> BuildTask { fatal : FatalError, recoverable : File.FileReadError decoderError } a)
    -> BuildTask { fatal : FatalError, recoverable : File.FileReadError decoderError } a
withFile hash f =
    BuildTask "withFile"
        (\({ buildPath } as input) state ->
            Do.do
                (BackendTask.File.Extra.read (Hash.toFilePath buildPath hash)
                    |> BackendTask.mapError UserError
                )
            <| \raw ->
            runMonad (f raw) input { deps = HashSet.insert hash state.deps, warnings = state.warnings }
        )


run :
    { jobs : Maybe Int
    , debug : Bool
    , check : Bool
    , hashKind : Hash.Kind
    , keepFailed : Bool
    }
    -> Path Path.Absolute Path.Directory
    -> BuildTask e (Hash Normal)
    ->
        BackendTask
            (Error e)
            { output : Path Path.Absolute Path.FileOrDirectory
            , intermediate : List (Path Path.Absolute Path.FileOrDirectory)
            , warnings : Set String
            }
run config buildPath m =
    Do.do
        (Script.makeDirectory { recursive = True } (Path.toString buildPath)
            |> BackendTask.mapError InternalError
        )
    <| \_ ->
    Do.do (listExisting buildPath |> BackendTask.mapError InternalError) <| \existing ->
    Do.do
        (case config.jobs of
            Nothing ->
                BackendTask.mapError InternalError (nproc { memoryLimit = Nothing, prefix = [], env = Dict.empty, idlePriority = False, debug = config.debug })

            Just j ->
                BackendTask.succeed j
        )
    <| \jobs_ ->
    let
        input : Input
        input =
            { existing = existing
            , prefix = []
            , buildPath = buildPath
            , jobs = jobs_
            , debug = config.debug
            , hashKind = config.hashKind
            , check = config.check
            , keepFailed = config.keepFailed
            , env = Dict.empty
            , memoryLimit = Nothing
            , idlePriority = False
            }
    in
    Do.do (runMonad m input { deps = HashSet.empty, warnings = Set.empty }) <| \( output, state ) ->
    { output = Hash.toFileOrDirectoryPath buildPath output
    , intermediate =
        state.deps
            |> HashSet.toList
            |> List.map (\raw -> raw |> Hash.toFileOrDirectoryPath buildPath)
    , warnings = state.warnings
    }
        |> BackendTask.succeed


listExisting : Path base Path.Directory -> BackendTask FatalError HashSet
listExisting path =
    BackendTask.File.Extra.listFilesAndDirectoriesIn path
        |> BackendTask.andThen
            (\( files, directories ) ->
                let
                    list : List String
                    list =
                        List.map Path.toString files ++ List.map Path.toString directories
                in
                case
                    list
                        |> List.Extra.removeWhen
                            (\s ->
                                String.startsWith "tmp-" s
                                    || String.startsWith "workspace-" s
                            )
                        |> HashSet.fromList
                of
                    Ok o ->
                        BackendTask.succeed o

                    Err e ->
                        BackendTask.fail (FatalError.fromString e)
            )


combineBy : Int -> List (BuildTask e a) -> BuildTask e (List a)
combineBy n ops =
    BuildTask "combineBy"
        (\input deps ->
            case ops of
                [] ->
                    BackendTask.succeed ( [], deps )

                _ :: _ ->
                    ops
                        |> List.map
                            (\m -> runMonad m input deps)
                        |> BackendTask.Extra.combineBy n
                        |> BackendTask.map
                            (\resList ->
                                resList
                                    |> List.unzip
                                    |> Tuple.mapSecond combineOutputs
                            )
        )


sequence : List (BuildTask e a) -> BuildTask e (List a)
sequence ops =
    BuildTask "sequence"
        (\input deps ->
            if List.isEmpty ops then
                BackendTask.succeed ( [], deps )

            else
                ops
                    |> List.map (\m -> runMonad m input deps)
                    |> BackendTask.sequence
                    |> BackendTask.map
                        (\res ->
                            res
                                |> List.unzip
                                |> Tuple.mapSecond combineOutputs
                        )
        )


timed : String -> String -> BuildTask e a -> BuildTask e a
timed before after task =
    BuildTask "timed" (\input deps -> BackendTask.Extra.timed before after (runMonad task input deps))


withPrefix : String -> BuildTask e a -> BuildTask e a
withPrefix newPrefix m =
    BuildTask "withPrefix" (\input deps -> runMonad m { input | prefix = newPrefix :: input.prefix } deps)


jobs : BuildTask e Int
jobs =
    BuildTask "jobs" (\input deps -> BackendTask.succeed ( input.jobs, deps ))


nproc :
    { input
        | memoryLimit : Maybe Int
        , prefix : List String
        , env : Dict String String
        , idlePriority : Bool
        , debug : Bool
    }
    -> BackendTask FatalError Int
nproc input =
    let
        tryRunningOrElse :
            String
            -> List String
            -> (() -> BackendTask FatalError Int)
            -> BackendTask FatalError Int
        tryRunningOrElse cmd args orElse =
            Do.do (commandUnlogged input cmd args |> BackendTask.toResult) <| \nprocResult ->
            case nprocResult of
                Ok output ->
                    let
                        trimmed : String
                        trimmed =
                            String.trim output
                    in
                    case String.toInt trimmed of
                        Nothing ->
                            let
                                message : String
                                message =
                                    "Invalid " ++ String.join " " (cmd :: args) ++ " output: " ++ Utils.escape trimmed
                            in
                            BackendTask.fail (FatalError.fromString message)

                        Just j ->
                            BackendTask.succeed j

                Err _ ->
                    orElse ()
    in
    tryRunningOrElse "nproc" [ "--all" ] <| \_ ->
    tryRunningOrElse "sysctl" [ "-n", "hw.logicalcpu" ] <| \_ ->
    let
        message : String
        message =
            "Failed to run either `nproc --all` or `sysctl -n hw.logicalcpu`. You can work around this by specifying an explicit number of jobs to run in parallel"
    in
    BackendTask.fail (FatalError.fromString message)


succeed : a -> BuildTask e a
succeed v =
    BuildTask "succeed" (\_ deps -> BackendTask.succeed ( v, deps ))


fail : e -> BuildTask e a
fail msg =
    BuildTask "checkIfDebug" (\{ debug } deps -> BackendTask.succeed ( debug, deps ))
        |> andThen
            (\debug ->
                if debug then
                    triggerDebugger

                else
                    succeed ()
            )
        |> andThen
            (\_ ->
                BuildTask "fail"
                    (\_ _ ->
                        BackendTask.fail (UserError msg)
                     -- (FatalError.build
                     --     { title = String.join " " (prefix ++ [ "-", "build error" ])
                     --     , body = msg
                     --     }
                     -- )
                    )
            )


withWarning : Warning -> BuildTask e a -> BuildTask e a
withWarning warning ((BuildTask n _) as t) =
    BuildTask n
        (\input deps ->
            runMonad t input deps
                |> BackendTask.map
                    (\( r, newState ) ->
                        ( r
                        , { deps = newState.deps
                          , warnings = Set.insert warning newState.warnings
                          }
                        )
                    )
        )


triggerDebugger : BuildTask e ()
triggerDebugger =
    BuildTask "triggerDebugger"
        (\_ deps ->
            BackendTask.Customs.triggerDebugger
                |> BackendTask.map (\() -> ( (), deps ))
                |> BackendTask.mapError InternalError
        )


inputFile : Path base Path.File -> BuildTask FatalError (Hash Normal)
inputFile inputPath =
    -- TODO: Copy files before hashing them
    BuildTask "input"
        (\input deps ->
            Do.do
                (commandLog input "b3sum" [ Path.toString inputPath ]
                    |> BackendTask.allowFatal
                    |> BackendTask.mapError UserError
                )
            <| \body ->
            case Hash.fromChecksum body of
                Err e ->
                    BackendTask.fail (UserError (FatalError.fromString e))

                Ok hash ->
                    BackendTask.succeed ( hash, deps )
        )
        |> andThen
            (\hash ->
                deriveFile "input" hash <| \_ target ->
                BackendTask.File.Extra.copyFile
                    { from = inputPath
                    , to = target
                    }
                    |> BackendTask.mapError UserError
            )


which : String -> BuildTask FatalError Command
which command =
    BuildTask "which"
        (\_ deps ->
            Do.do
                (Script.expectWhich command
                    |> BackendTask.mapError UserError
                )
            <| \path ->
            Do.do
                (Script.command "b3sum" [ path ]
                    |> BackendTask.mapError InternalError
                )
            <| \line ->
            case Hash.fromChecksum line of
                Ok hash ->
                    BackendTask.succeed ( { name = command, hash = hash }, deps )

                Err e ->
                    BackendTask.fail (InternalError (FatalError.fromString e))
        )


type DownloadError
    = WrongHashLength String
    | InvalidHashHex String
    | DownloadError FatalError
    | WrongHash { expected : String, actual : String }


downloadSHA256 : { url : String, sha256 : String } -> BuildTask DownloadError (Hash Normal)
downloadSHA256 { url, sha256 } =
    let
        hashLength : Int
        hashLength =
            String.length sha256
    in
    if hashLength /= 64 then
        fail (WrongHashLength sha256)

    else
        case XBytes.fromHex sha256 of
            Nothing ->
                fail (InvalidHashHex sha256)

            Just _ ->
                deriveFile "downloadSHA256" (Hash.build sha256) <| \input target ->
                let
                    tmpPath : String
                    tmpPath =
                        Path.toString target
                in
                Do.do
                    (Stream.http
                        { url = url
                        , method = "GET"
                        , headers = []
                        , body = Http.emptyBody
                        , retries = Nothing
                        , timeoutInMs = Nothing
                        }
                        |> Stream.pipe (Stream.fileWrite tmpPath)
                        |> Stream.run
                        |> BackendTask.mapError (\e -> UserError (DownloadError e))
                    )
                <| \_ ->
                Do.do
                    (commandLog input "sha256sum" [ tmpPath ]
                        |> BackendTask.allowFatal
                        |> BackendTask.mapError InternalError
                    )
                <| \body ->
                if String.startsWith sha256 body then
                    BackendTask.succeed ()

                else
                    BackendTask.fail
                        (UserError
                            (WrongHash
                                { expected = sha256
                                , actual = String.left 64 body
                                }
                            )
                        )


{-| -}
commandLog :
    { input
        | memoryLimit : Maybe Int
        , prefix : List String
        , env : Dict String String
        , idlePriority : Bool
        , debug : Bool
    }
    -> String
    -> List String
    -> BackendTask { fatal : FatalError, recoverable : Stream.Error Int String } String
commandLog input cmd args =
    wrapCommand input BuildTask.CommandOptions.default cmd args
        |> Stream.read
        |> BackendTask.map .body
        |> logCommand LogAlways input cmd args


{-| -}
commandUnlogged :
    { input
        | memoryLimit : Maybe Int
        , prefix : List String
        , env : Dict String String
        , idlePriority : Bool
        , debug : Bool
    }
    -> String
    -> List String
    -> BackendTask { fatal : FatalError, recoverable : Stream.Error Int String } String
commandUnlogged input cmd args =
    wrapCommand input BuildTask.CommandOptions.default cmd args
        |> Stream.read
        |> BackendTask.map .body
        |> logCommand LogIfDebug input cmd args


{-| -}
commandLogWith :
    { input
        | memoryLimit : Maybe Int
        , prefix : List String
        , env : Dict String String
        , idlePriority : Bool
        , debug : Bool
    }
    -> CommandOptions
    -> String
    -> List String
    -> BackendTask { fatal : FatalError, recoverable : Stream.Error Int String } String
commandLogWith input options cmd args =
    wrapCommand input options cmd args
        |> Stream.read
        |> BackendTask.map .body
        |> logCommand LogAlways input cmd args


{-| -}
execLog : Input -> String -> List String -> BackendTask FatalError ()
execLog input cmd args =
    wrapCommand input BuildTask.CommandOptions.default cmd args
        |> Stream.run
        |> logCommand LogAlways input cmd args


execUnlogged : Input -> String -> List String -> BackendTask FatalError ()
execUnlogged input cmd args =
    wrapCommand input BuildTask.CommandOptions.default cmd args
        |> Stream.run
        |> logCommand LogIfDebug input cmd args


wrapCommand :
    { input
        | memoryLimit : Maybe Int
        , idlePriority : Bool
    }
    -> CommandOptions
    -> String
    -> List String
    -> Stream Int () { read : read, write : write }
wrapCommand input options cmd args =
    let
        memoryArg : String
        memoryArg =
            case input.memoryLimit of
                Nothing ->
                    ""

                Just limit ->
                    let
                        kilo : Int
                        kilo =
                            1024

                        mega : Int
                        mega =
                            kilo * 1024

                        giga : Int
                        giga =
                            mega * 1024
                    in
                    if modBy giga limit == 0 then
                        String.fromInt (limit // giga) ++ "G"

                    else if modBy mega limit == 0 then
                        String.fromInt (limit // mega) ++ "M"

                    else if modBy kilo limit == 0 then
                        String.fromInt (limit // kilo) ++ "K"

                    else
                        String.fromInt limit

        cpuWeightArg : String
        cpuWeightArg =
            if input.idlePriority then
                "CPUWeight=idle"

            else
                ""

        ( streamOptions, timeout ) =
            BuildTask.CommandOptions.toStreamCommandOptionsAndTimeout options
    in
    if String.isEmpty memoryArg && String.isEmpty cpuWeightArg then
        case timeout of
            Nothing ->
                Stream.commandWithOptions streamOptions cmd args

            Just duration ->
                Stream.commandWithOptions streamOptions
                    "timeout"
                    (String.fromFloat (Duration.inSeconds duration)
                        :: cmd
                        :: args
                    )

    else
        ([ "--quiet"
         , "--user"
         , "--scope"
         ]
            ++ (if String.isEmpty memoryArg then
                    []

                else
                    [ "-p", "MemoryMax=" ++ memoryArg ]
               )
            ++ (if String.isEmpty cpuWeightArg then
                    []

                else
                    [ "-p", cpuWeightArg ]
               )
            ++ (case timeout of
                    Nothing ->
                        [ "--", cmd ]

                    Just duration ->
                        [ "--"
                        , "timeout"
                        , String.fromFloat (Duration.inSeconds duration)
                        , cmd
                        ]
               )
            ++ args
        )
            |> Stream.commandWithOptions streamOptions "systemd-run"


type WhenToLog
    = LogAlways
    | LogIfDebug


{-| -}
logCommand : WhenToLog -> { a | prefix : List String, env : Dict String String, debug : Bool } -> String -> List String -> BackendTask error b -> BackendTask error b
logCommand when { prefix, env, debug } cmd args task =
    let
        label : String -> String
        label i =
            (prefix
                ++ String.padLeft 7 ' ' i
                :: Utils.viewEnv env
                :: cmd
                :: args
            )
                |> List.Extra.removeWhen String.isEmpty
                |> String.join " "

        log =
            case when of
                LogAlways ->
                    True

                LogIfDebug ->
                    debug
    in
    if log then
        BackendTask.Extra.timed
            (label "Running")
            (label "Ran")
            task

    else
        task


named :
    String
    -> (a -> { files : List (Hash Normal), additionalData : List String })
    -> (a -> BuildTask e (Hash Normal))
    -> a
    -> BuildTask e (Hash Normal)
named name encode action param =
    let
        encoded : { files : List (Hash Normal), additionalData : List String }
        encoded =
            encode param
    in
    (name :: encoded.additionalData ++ List.map Hash.toString encoded.files)
        |> String.join "|"
        |> hashFromString
        |> andThen
            (\hash ->
                BuildTask "named"
                    (\input state ->
                        if HashSet.member hash input.existing || HashSet.member hash state.deps then
                            ( hash
                            , { deps = HashSet.insert hash state.deps
                              , warnings = state.warnings
                              }
                            )
                                |> BackendTask.succeed

                        else
                            runMonad (action param) input state
                                |> BackendTask.andThen
                                    (\( actionOutput, newState ) ->
                                        Do.do
                                            (BackendTask.File.Extra.move
                                                { from = Hash.toFilePath input.buildPath actionOutput
                                                , to = Hash.toFilePath input.buildPath hash
                                                }
                                                |> BackendTask.mapError InternalError
                                            )
                                        <| \_ ->
                                        BackendTask.succeed
                                            ( hash
                                            , { deps = HashSet.insert hash newState.deps
                                              , warnings = newState.warnings
                                              }
                                            )
                                    )
                    )
            )


extendHashWith : List String -> Hash Normal -> BuildTask e (Hash Normal)
extendHashWith l r =
    hashFromString (String.join "|" (Hash.toString r :: l))


hashFromString : String -> BuildTask e (Hash Normal)
hashFromString raw =
    BuildTask "fromString"
        (\{ hashKind } deps ->
            BackendTask.succeed ( Hash.fromString raw hashKind, deps )
        )


toResult : BuildTask e a -> BuildTask x (Result e a)
toResult ((BuildTask name _) as f) =
    BuildTask name
        (\input deps ->
            runMonad f input deps
                |> BackendTask.toResult
                |> BackendTask.andThen
                    (\res ->
                        case res of
                            Ok ( o, newDeps ) ->
                                BackendTask.succeed ( Ok o, newDeps )

                            Err (UserError e) ->
                                BackendTask.succeed ( Err e, deps )

                            Err (InternalError e) ->
                                BackendTask.fail (InternalError e)
                    )
        )


withEnv : List ( String, String ) -> BuildTask e a -> BuildTask e a
withEnv env (BuildTask name f) =
    BuildTask name
        (\input state ->
            f { input | env = Dict.union (Dict.fromList env) input.env } state
        )


withMemoryLimitInBytes : Int -> BuildTask e a -> BuildTask e a
withMemoryLimitInBytes limit (BuildTask name f) =
    BuildTask name
        (\input state ->
            f { input | memoryLimit = Just limit } state
        )


withDebug : (String -> Never) -> BuildTask e a -> BuildTask e a
withDebug _ (BuildTask name f) =
    BuildTask name
        (\input state ->
            f { input | debug = True } state
        )


withIdlePriority : BuildTask e a -> BuildTask e a
withIdlePriority (BuildTask name f) =
    BuildTask name
        (\input state ->
            f { input | idlePriority = True } state
        )


mapError :
    (e -> f)
    -> BuildTask e a
    -> BuildTask f a
mapError f ((BuildTask name _) as t) =
    BuildTask name
        (\input state ->
            runMonad t input state
                |> BackendTask.mapError
                    (\e ->
                        case e of
                            InternalError i ->
                                InternalError i

                            UserError u ->
                                UserError (f u)
                    )
        )


allowFatal : BuildTask { e | fatal : FatalError } a -> BuildTask FatalError a
allowFatal ((BuildTask name _) as t) =
    BuildTask name
        (\input state ->
            runMonad t input state
                |> BackendTask.mapError
                    (\e ->
                        case e of
                            InternalError i ->
                                InternalError i

                            UserError { fatal } ->
                                UserError fatal
                    )
        )


fatalToInternal : BuildTask FatalError a -> BuildTask e a
fatalToInternal ((BuildTask name _) as t) =
    BuildTask name
        (\input state ->
            runMonad t input state
                |> BackendTask.mapError
                    (\err ->
                        case err of
                            UserError u ->
                                InternalError u

                            InternalError i ->
                                InternalError i
                    )
        )


extractFromDirectory : Hash Normal -> String -> BuildTask { fatal : FatalError, recoverable : File.FileReadError e } (Hash Normal)
extractFromDirectory directory file =
    extendHashWith [ "extract", file ] directory
        |> andThen
            (\outputHash ->
                deriveFile ("extract " ++ file) outputHash <| \{ buildPath } target ->
                case Path.parseRelativeFile file of
                    Nothing ->
                        UserError
                            { fatal = FatalError.fromString ("Invalid filename: " ++ file)
                            , recoverable = File.FileDoesntExist
                            }
                            |> BackendTask.fail

                    Just filename ->
                        BackendTask.File.Extra.copyFile
                            { from =
                                Path.append
                                    (Hash.toDirectoryPath buildPath directory)
                                    filename
                            , to = target
                            }
                            |> BackendTask.mapError
                                (\fatal ->
                                    UserError
                                        { fatal = fatal
                                        , recoverable = File.FileDoesntExist
                                        }
                                )
            )
