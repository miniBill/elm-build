module BuildTask.Internal exposing (BuildTask(..), Command, DownloadError(..), Error(..), Input, InternalTools, State, Warning, allowFatal, andThen, andThen2, combineBy, commandLog, commandLogWith, derive, downloadSHA256, execLog, execUnlogged, extendHashWith, extractFromDirectory, fail, fatalToInternal, getInternalTools, getTool, hashFromString, input, jobs, map, map2, map3, map4, map5, mapError, named, run, sequence, succeed, timed, toResult, triggerDebugger, which, withDebug, withEnv, withFile, withIdlePriority, withMemoryLimitInBytes, withPrefix, withWarning)

import BackendTask exposing (BackendTask)
import BackendTask.Customs
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.File.Extra
import BackendTask.Http as Http
import BackendTask.Stream as Stream exposing (Stream)
import CommandOptions exposing (CommandOptions)
import Duration
import FastDict as Dict exposing (Dict)
import FastSet as Set exposing (Set)
import FatalError exposing (FatalError)
import Hash exposing (Hash, Normal, Temporary)
import HashSet exposing (HashSet)
import List.Extra
import Pages.Script as Script
import Path exposing (Path)
import Utils
import XBytes


type BuildTask tools e a
    = BuildTask String (Input tools -> State -> BackendTask (Error e) ( a, State ))


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


type alias Input tools =
    { existing : HashSet
    , prefix : List String
    , buildPath : Path
    , jobs : Int
    , debug : Bool
    , check : Bool
    , keepFailed : Bool
    , idlePriority : Bool
    , hashKind : Hash.Kind
    , env : Dict String String
    , memoryLimit : Maybe Int
    , internalTools : InternalTools
    , tools : tools
    }


type alias InternalTools =
    { b3sum : Command
    , chmod : Command
    , cp : Command
    , diff : Command
    }


{-| Core primitive.

**CORRECTNESS:**

  - The task must be deterministic,
  - the hash must contain all the information the task depends on.

-}
derive :
    String
    -> Hash Normal
    ->
        (Input tools
         -> Hash Temporary
         -> BackendTask (Error e) ()
        )
    -> BuildTask tools e (Hash Normal)
derive description target inner =
    BuildTask "derive"
        (\({ internalTools, existing, buildPath } as input_) state ->
            let
                newDeps : HashSet
                newDeps =
                    HashSet.insert target state.deps

                appendLog : BackendTask (Error e) ()
                appendLog =
                    if input_.debug then
                        (Hash.toPath buildPath target
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
            Do.do appendLog <|
                \_ ->
                    if (HashSet.member target existing && not input_.check) || HashSet.member target state.deps then
                        BackendTask.succeed ( target, { deps = newDeps, warnings = state.warnings } )

                    else
                        let
                            targetPath : String
                            targetPath =
                                Hash.toPath buildPath target
                        in
                        Do.do (File.exists targetPath) <|
                            \exists ->
                                if exists && not input_.check && not input_.debug then
                                    BackendTask.succeed ( target, { deps = newDeps, warnings = state.warnings } )

                                else
                                    let
                                        tmp : Hash Temporary
                                        tmp =
                                            Hash.toTemporary target

                                        tmpPath : String
                                        tmpPath =
                                            Hash.toPathTemporary buildPath tmp
                                    in
                                    Do.do
                                        (BackendTask.File.Extra.removeFileIfExists tmpPath
                                            |> BackendTask.mapError InternalError
                                        )
                                    <|
                                        \_ ->
                                            Do.do
                                                (inner input_ tmp
                                                    |> BackendTask.onError
                                                        (\e ->
                                                            Do.do
                                                                (BackendTask.File.Extra.removeFileIfExists tmpPath
                                                                    |> BackendTask.mapError InternalError
                                                                )
                                                            <|
                                                                \_ ->
                                                                    BackendTask.fail e
                                                        )
                                                )
                                            <|
                                                \_ ->
                                                    Do.do
                                                        (if exists then
                                                            Do.do
                                                                (execUnlogged input_ internalTools.diff.name [ "-r", tmpPath, targetPath ]
                                                                    |> BackendTask.onError
                                                                        (\e ->
                                                                            Do.log ("Error inside " ++ description) <|
                                                                                \_ ->
                                                                                    BackendTask.fail (InternalError e)
                                                                        )
                                                                )
                                                            <|
                                                                \_ ->
                                                                    BackendTask.File.Extra.removeFileIfExists tmpPath
                                                                        |> BackendTask.mapError InternalError

                                                         else
                                                            Script.move { from = tmpPath, to = targetPath }
                                                                |> BackendTask.onError
                                                                    (\e ->
                                                                        Do.log ("Error inside " ++ description) <|
                                                                            \_ ->
                                                                                BackendTask.fail (InternalError e)
                                                                    )
                                                        )
                                                    <|
                                                        \_ ->
                                                            Do.do
                                                                (execUnlogged input_ "chmod" [ "-R", "a=rX", targetPath ]
                                                                    |> BackendTask.mapError InternalError
                                                                )
                                                            <|
                                                                \_ ->
                                                                    BackendTask.succeed ( target, { deps = newDeps, warnings = state.warnings } )
        )


runMonad : BuildTask tools e a -> Input tools -> State -> BackendTask (Error e) ( a, State )
runMonad (BuildTask label m) input_ state =
    if input_.debug then
        Dict.foldl BackendTask.withEnv (m input_ state) input_.env
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
        Dict.foldl BackendTask.withEnv (m input_ state) input_.env


{-| -}
map : (a -> b) -> BuildTask tools e a -> BuildTask tools e b
map f m =
    BuildTask "map"
        (\input_ deps ->
            runMonad m input_ deps
                |> BackendTask.map
                    (\( v, output ) ->
                        ( f v, output )
                    )
        )


{-| -}
map2 :
    (a -> b -> c)
    -> BuildTask tools e a
    -> BuildTask tools e b
    -> BuildTask tools e c
map2 f a b =
    BuildTask "map2"
        (\input_ deps ->
            BackendTask.map2
                (\( va, outputA ) ( vb, outputB ) ->
                    ( f va vb, combineOutput outputA outputB )
                )
                (runMonad a input_ deps)
                (runMonad b input_ deps)
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
    -> BuildTask tools e a
    -> BuildTask tools e b
    -> BuildTask tools e c
    -> BuildTask tools e d
map3 f a b c =
    BuildTask "map3"
        (\input_ deps ->
            BackendTask.map3
                (\( va, outputA ) ( vb, outputB ) ( vc, outputC ) ->
                    ( f va vb vc, combineOutputs [ outputA, outputB, outputC ] )
                )
                (runMonad a input_ deps)
                (runMonad b input_ deps)
                (runMonad c input_ deps)
        )


{-| -}
map4 :
    (a -> b -> c -> d -> r)
    -> BuildTask tools e a
    -> BuildTask tools e b
    -> BuildTask tools e c
    -> BuildTask tools e d
    -> BuildTask tools e r
map4 f a b c d =
    BuildTask "map4"
        (\input_ deps ->
            BackendTask.map4
                (\( va, outputA ) ( vb, outputB ) ( vc, outputC ) ( vd, outputD ) ->
                    ( f va vb vc vd
                    , combineOutputs [ outputA, outputB, outputC, outputD ]
                    )
                )
                (runMonad a input_ deps)
                (runMonad b input_ deps)
                (runMonad c input_ deps)
                (runMonad d input_ deps)
        )


{-| -}
map5 :
    (a -> b -> c -> d -> f -> g)
    -> BuildTask tools e a
    -> BuildTask tools e b
    -> BuildTask tools e c
    -> BuildTask tools e d
    -> BuildTask tools e f
    -> BuildTask tools e g
map5 f a b c d e =
    BuildTask "map5"
        (\input_ deps ->
            BackendTask.map5
                (\( va, outputA ) ( vb, outputB ) ( vc, outputC ) ( vd, outputD ) ( ve, outputE ) ->
                    ( f va vb vc vd ve
                    , combineOutputs [ outputA, outputB, outputC, outputD, outputE ]
                    )
                )
                (runMonad a input_ deps)
                (runMonad b input_ deps)
                (runMonad c input_ deps)
                (runMonad d input_ deps)
                (runMonad e input_ deps)
        )


{-| -}
andThen : (a -> BuildTask tools e b) -> BuildTask tools e a -> BuildTask tools e b
andThen f m =
    BuildTask "andThen"
        (\input_ deps ->
            runMonad m input_ deps
                |> BackendTask.andThen
                    (\( v, newDeps ) ->
                        runMonad (f v) input_ newDeps
                    )
        )


{-| -}
andThen2 : (a -> b -> BuildTask tools e c) -> BuildTask tools e a -> BuildTask tools e b -> BuildTask tools e c
andThen2 f l r =
    BuildTask "andThen2"
        (\input_ deps ->
            BackendTask.map2
                Tuple.pair
                (runMonad l input_ deps)
                (runMonad r input_ deps)
                |> BackendTask.andThen
                    (\( ( lv, lOutput ), ( rv, rOutput ) ) ->
                        runMonad (f lv rv) input_ (combineOutput lOutput rOutput)
                    )
        )


withFile :
    Hash Normal
    -> (String -> BuildTask tools { fatal : FatalError, recoverable : File.FileReadError decoderError } a)
    -> BuildTask tools { fatal : FatalError, recoverable : File.FileReadError decoderError } a
withFile hash f =
    BuildTask "withFile"
        (\({ buildPath } as input_) state ->
            Do.do
                (File.rawFile (Hash.toPath buildPath hash)
                    |> BackendTask.mapError UserError
                )
            <|
                \raw ->
                    runMonad (f raw) input_ { deps = HashSet.insert hash state.deps, warnings = state.warnings }
        )


run :
    { jobs : Maybe Int
    , debug : Bool
    , check : Bool
    , hashKind : Hash.Kind
    , keepFailed : Bool
    , getTools : BuildTask () e tools
    }
    -> Path
    -> BuildTask tools e (Hash Normal)
    -> BackendTask (Error e) { output : Path, intermediate : List Path, warnings : Set String }
run config buildPath m =
    Do.do
        (Script.makeDirectory { recursive = True } (Path.toString buildPath)
            |> BackendTask.mapError InternalError
        )
    <|
        \_ ->
            Do.do (listExisting buildPath |> BackendTask.mapError InternalError) <|
                \existing ->
                    Do.do
                        (case config.jobs of
                            Nothing ->
                                BackendTask.mapError InternalError (nproc { memoryLimit = Nothing, prefix = [], env = Dict.empty, idlePriority = False, debug = config.debug })

                            Just j ->
                                BackendTask.succeed j
                        )
                    <|
                        \jobs_ ->
                            Do.do (getInternalTools |> BackendTask.mapError InternalError) <|
                                \internalTools ->
                                    let
                                        input_ : t -> Input t
                                        input_ tools =
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
                                            , tools = tools
                                            , internalTools = internalTools
                                            }
                                    in
                                    Do.do (runMonad config.getTools (input_ ()) { deps = HashSet.empty, warnings = Set.empty }) <|
                                        \( tools, intermediate ) ->
                                            Do.do (runMonad m (input_ tools) intermediate) <|
                                                \( output, state ) ->
                                                    { output = Path.path (Hash.toPath buildPath output)
                                                    , intermediate =
                                                        state.deps
                                                            |> HashSet.toList
                                                            |> List.map (\raw -> raw |> Hash.toPath buildPath |> Path.path)
                                                    , warnings = state.warnings
                                                    }
                                                        |> BackendTask.succeed


getInternalTools : BackendTask FatalError InternalTools
getInternalTools =
    BackendTask.map4 InternalTools
        (which_ "b3sum")
        (which_ "chmod")
        (which_ "cp")
        (which_ "diff")


listExisting : Path -> BackendTask FatalError HashSet
listExisting path =
    BackendTask.Customs.readdir path
        |> BackendTask.andThen
            (\list ->
                case
                    HashSet.fromList
                        (List.Extra.removeWhen
                            (\s ->
                                String.startsWith "tmp-" s
                                    || String.startsWith "workspace-" s
                            )
                            list
                        )
                of
                    Ok o ->
                        BackendTask.succeed o

                    Err e ->
                        BackendTask.fail (FatalError.fromString e)
            )


combineBy : Int -> List (BuildTask tools e a) -> BuildTask tools e (List a)
combineBy n ops =
    BuildTask "combineBy"
        (\input_ deps ->
            case ops of
                [] ->
                    BackendTask.succeed ( [], deps )

                _ :: _ ->
                    ops
                        |> List.map
                            (\m -> runMonad m input_ deps)
                        |> BackendTask.Extra.combineBy n
                        |> BackendTask.map
                            (\resList ->
                                resList
                                    |> List.unzip
                                    |> Tuple.mapSecond combineOutputs
                            )
        )


sequence : List (BuildTask tools e a) -> BuildTask tools e (List a)
sequence ops =
    BuildTask "sequence"
        (\input_ deps ->
            if List.isEmpty ops then
                BackendTask.succeed ( [], deps )

            else
                ops
                    |> List.map (\m -> runMonad m input_ deps)
                    |> BackendTask.sequence
                    |> BackendTask.map
                        (\res ->
                            res
                                |> List.unzip
                                |> Tuple.mapSecond combineOutputs
                        )
        )


timed : String -> String -> BuildTask tools e a -> BuildTask tools e a
timed before after task =
    BuildTask "timed" (\input_ deps -> BackendTask.Extra.timed before after (runMonad task input_ deps))


withPrefix : String -> BuildTask tools e a -> BuildTask tools e a
withPrefix newPrefix m =
    BuildTask "withPrefix" (\input_ deps -> runMonad m { input_ | prefix = newPrefix :: input_.prefix } deps)


jobs : BuildTask tools e Int
jobs =
    BuildTask "jobs" (\input_ deps -> BackendTask.succeed ( input_.jobs, deps ))


nproc :
    { input
        | memoryLimit : Maybe Int
        , prefix : List String
        , env : Dict String String
        , idlePriority : Bool
        , debug : Bool
    }
    -> BackendTask FatalError Int
nproc input_ =
    let
        tryRunningOrElse :
            String
            -> List String
            -> (() -> BackendTask FatalError Int)
            -> BackendTask FatalError Int
        tryRunningOrElse cmd args orElse =
            Do.do (commandUnlogged input_ cmd args |> BackendTask.toResult) <|
                \nprocResult ->
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
    tryRunningOrElse "nproc" [ "--all" ] <|
        \_ ->
            tryRunningOrElse "sysctl" [ "-n", "hw.logicalcpu" ] <|
                \_ ->
                    let
                        message : String
                        message =
                            "Failed to run either `nproc --all` or `sysctl -n hw.logicalcpu`. You can work around this by specifying an explicit number of jobs to run in parallel"
                    in
                    BackendTask.fail (FatalError.fromString message)


succeed : a -> BuildTask tools e a
succeed v =
    BuildTask "succeed" (\_ deps -> BackendTask.succeed ( v, deps ))


fail : e -> BuildTask tools e a
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


withWarning : Warning -> BuildTask tools e a -> BuildTask tools e a
withWarning warning ((BuildTask n _) as t) =
    BuildTask n
        (\input_ deps ->
            runMonad t input_ deps
                |> BackendTask.map
                    (\( r, newState ) ->
                        ( r
                        , { deps = newState.deps
                          , warnings = Set.insert warning newState.warnings
                          }
                        )
                    )
        )


triggerDebugger : BuildTask tools e ()
triggerDebugger =
    BuildTask "triggerDebugger"
        (\_ deps ->
            BackendTask.Customs.triggerDebugger
                |> BackendTask.map (\() -> ( (), deps ))
                |> BackendTask.mapError InternalError
        )


input : Path -> BuildTask tools FatalError (Hash Normal)
input inputPath =
    -- TODO: Copy files before hashing them
    BuildTask "input"
        (\input_ deps ->
            Do.do
                (commandLog input_ input_.internalTools.b3sum.name [ Path.toString inputPath ]
                    |> BackendTask.allowFatal
                    |> BackendTask.mapError UserError
                )
            <|
                \body ->
                    case Hash.fromChecksum body of
                        Err e ->
                            BackendTask.fail (UserError (FatalError.fromString e))

                        Ok hash ->
                            BackendTask.succeed ( hash, deps )
        )
        |> andThen
            (\hash ->
                derive "input" hash <|
                    \{ buildPath } target ->
                        Script.copyFile { from = Path.toString inputPath, to = Hash.toPathTemporary buildPath target }
                            |> BackendTask.mapError UserError
            )


which : String -> BuildTask tools FatalError Command
which command =
    BuildTask "which"
        (\_ deps ->
            Do.do
                (Script.expectWhich command
                    |> BackendTask.mapError UserError
                )
            <|
                \path ->
                    Do.do
                        (Script.command "b3sum" [ path ]
                            |> BackendTask.mapError InternalError
                        )
                    <|
                        \line ->
                            case Hash.fromChecksum line of
                                Ok hash ->
                                    BackendTask.succeed ( { name = command, hash = hash }, deps )

                                Err e ->
                                    BackendTask.fail (InternalError (FatalError.fromString e))
        )


which_ : String -> BackendTask FatalError Command
which_ name =
    Do.do (Script.expectWhich name) <|
        \path ->
            Do.command "b3sum" [ path ] <|
                \line ->
                    case Hash.fromChecksum line of
                        Ok hash ->
                            BackendTask.succeed { name = name, hash = hash }

                        Err e ->
                            BackendTask.fail (FatalError.fromString e)


type DownloadError
    = WrongHashLength String
    | InvalidHashHex String
    | DownloadError FatalError
    | WrongHash { expected : String, actual : String }


downloadSHA256 : { url : String, sha256 : String } -> BuildTask { tools | sha256sum : Command } DownloadError (Hash Normal)
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
                derive "downloadSHA256" (Hash.build sha256) <|
                    \({ tools, buildPath } as input_) target ->
                        let
                            tmpPath : String
                            tmpPath =
                                Hash.toPathTemporary buildPath target
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
                        <|
                            \_ ->
                                Do.do
                                    (commandLog input_ tools.sha256sum.name [ tmpPath ]
                                        |> BackendTask.allowFatal
                                        |> BackendTask.mapError InternalError
                                    )
                                <|
                                    \body ->
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
commandLog input_ cmd args =
    wrapCommand input_ CommandOptions.default cmd args
        |> Stream.read
        |> BackendTask.map .body
        |> logCommand LogAlways input_ cmd args


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
commandUnlogged input_ cmd args =
    wrapCommand input_ CommandOptions.default cmd args
        |> Stream.read
        |> BackendTask.map .body
        |> logCommand LogIfDebug input_ cmd args


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
commandLogWith input_ options cmd args =
    wrapCommand input_ options cmd args
        |> Stream.read
        |> BackendTask.map .body
        |> logCommand LogAlways input_ cmd args


{-| -}
execLog : Input tools -> String -> List String -> BackendTask FatalError ()
execLog input_ cmd args =
    wrapCommand input_ CommandOptions.default cmd args
        |> Stream.run
        |> logCommand LogAlways input_ cmd args


execUnlogged : Input tools -> String -> List String -> BackendTask FatalError ()
execUnlogged input_ cmd args =
    wrapCommand input_ CommandOptions.default cmd args
        |> Stream.run
        |> logCommand LogIfDebug input_ cmd args


wrapCommand :
    { input
        | memoryLimit : Maybe Int
        , idlePriority : Bool
    }
    -> CommandOptions
    -> String
    -> List String
    -> Stream Int () { read : read, write : write }
wrapCommand input_ options cmd args =
    let
        memoryArg : String
        memoryArg =
            case input_.memoryLimit of
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
            if input_.idlePriority then
                "CPUWeight=idle"

            else
                ""

        ( streamOptions, timeout ) =
            CommandOptions.toStreamCommandOptionsAndTimeout options
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


named : String -> (a -> { files : List (Hash Normal), additionalData : List String }) -> (a -> BuildTask tools e (Hash Normal)) -> a -> BuildTask tools e (Hash Normal)
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
                    (\input_ state ->
                        if HashSet.member hash input_.existing || HashSet.member hash state.deps then
                            ( hash
                            , { deps = HashSet.insert hash state.deps
                              , warnings = state.warnings
                              }
                            )
                                |> BackendTask.succeed

                        else
                            runMonad (action param) input_ state
                                |> BackendTask.andThen
                                    (\( actionOutput, newState ) ->
                                        Do.do
                                            (Script.move
                                                { from = Hash.toPath input_.buildPath actionOutput
                                                , to = Hash.toPath input_.buildPath hash
                                                }
                                                |> BackendTask.mapError InternalError
                                            )
                                        <|
                                            \_ ->
                                                BackendTask.succeed
                                                    ( hash
                                                    , { deps = HashSet.insert hash newState.deps
                                                      , warnings = newState.warnings
                                                      }
                                                    )
                                    )
                    )
            )


extendHashWith : List String -> Hash Normal -> BuildTask tools e (Hash Normal)
extendHashWith l r =
    hashFromString (String.join "|" (Hash.toString r :: l))


hashFromString : String -> BuildTask tools e (Hash Normal)
hashFromString raw =
    BuildTask "fromString"
        (\{ hashKind } deps ->
            BackendTask.succeed ( Hash.fromString raw hashKind, deps )
        )


toResult : BuildTask tools e a -> BuildTask tools x (Result e a)
toResult ((BuildTask name _) as f) =
    BuildTask name
        (\input_ deps ->
            runMonad f input_ deps
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


withEnv : List ( String, String ) -> BuildTask tools e a -> BuildTask tools e a
withEnv env (BuildTask name f) =
    BuildTask name
        (\input_ state ->
            f { input_ | env = Dict.union (Dict.fromList env) input_.env } state
        )


withMemoryLimitInBytes : Int -> BuildTask tools e a -> BuildTask tools e a
withMemoryLimitInBytes limit (BuildTask name f) =
    BuildTask name
        (\input_ state ->
            f { input_ | memoryLimit = Just limit } state
        )


withDebug : (String -> Never) -> BuildTask tools e a -> BuildTask tools e a
withDebug _ (BuildTask name f) =
    BuildTask name
        (\input_ state ->
            f { input_ | debug = True } state
        )


withIdlePriority : BuildTask tools e a -> BuildTask tools e a
withIdlePriority (BuildTask name f) =
    BuildTask name
        (\input_ state ->
            f { input_ | idlePriority = True } state
        )


mapError :
    (e -> f)
    -> BuildTask tools e a
    -> BuildTask tools f a
mapError f ((BuildTask name _) as t) =
    BuildTask name
        (\input_ state ->
            runMonad t input_ state
                |> BackendTask.mapError
                    (\e ->
                        case e of
                            InternalError i ->
                                InternalError i

                            UserError u ->
                                UserError (f u)
                    )
        )


allowFatal : BuildTask tools { e | fatal : FatalError } a -> BuildTask tools FatalError a
allowFatal ((BuildTask name _) as t) =
    BuildTask name
        (\input_ state ->
            runMonad t input_ state
                |> BackendTask.mapError
                    (\e ->
                        case e of
                            InternalError i ->
                                InternalError i

                            UserError { fatal } ->
                                UserError fatal
                    )
        )


fatalToInternal : BuildTask tools FatalError a -> BuildTask tools e a
fatalToInternal ((BuildTask name _) as t) =
    BuildTask name
        (\input_ state ->
            runMonad t input_ state
                |> BackendTask.mapError
                    (\err ->
                        case err of
                            UserError u ->
                                InternalError u

                            InternalError i ->
                                InternalError i
                    )
        )


extractFromDirectory : Hash Normal -> String -> BuildTask tools { fatal : FatalError, recoverable : File.FileReadError e } (Hash Normal)
extractFromDirectory directory file =
    extendHashWith [ "extract", file ] directory
        |> andThen
            (\outputHash ->
                derive ("extract " ++ file) outputHash <|
                    \{ buildPath } target ->
                        Script.copyFile
                            { from = Hash.toPath buildPath directory ++ "/" ++ file
                            , to = Hash.toPathTemporary buildPath target
                            }
                            |> BackendTask.mapError
                                (\fatal ->
                                    UserError
                                        { fatal = fatal
                                        , recoverable = File.FileDoesntExist
                                        }
                                )
            )


getTool : (tools -> command) -> BuildTask tools e command
getTool getter =
    BuildTask "getTool" (\{ tools } state -> BackendTask.succeed ( getter tools, state ))
