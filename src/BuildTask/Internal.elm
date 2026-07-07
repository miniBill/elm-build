module BuildTask.Internal exposing (BuildTask(..), DownloadError(..), Error(..), Input, State, Warning, allowFatal, andThen, andThen2, combineBy, commandLog, commandLogWith, derive, downloadSHA256, execLog, extendHashWith, extractFromDirectory, fail, fatalToInternal, hashFromString, input, jobs, map, map2, map3, map4, map5, mapError, named, run, sequence, succeed, timed, toResult, triggerDebugger, withEnv, withFile, withPrefix, withWarning)

import BackendTask exposing (BackendTask)
import BackendTask.Customs
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.File.Extra
import BackendTask.Http as Http
import BackendTask.Stream as Stream
import CommandOptions exposing (CommandOptions)
import FastSet as Set exposing (Set)
import FatalError exposing (FatalError)
import Hash exposing (Hash, Normal, Temporary)
import HashSet exposing (HashSet)
import Hex
import Json.Encode
import List.Extra
import Pages.Script as Script
import Path exposing (Path)


type BuildTask e a
    = BuildTask String (Input -> State -> BackendTask (Error e) ( a, State ))


type Error e
    = InternalError FatalError
    | UserError e


type alias State =
    { deps : HashSet
    , warnings : Set String
    }


type alias Warning =
    String


type alias Input =
    { existing : HashSet
    , prefix : List String
    , buildPath : Path
    , jobs : Int
    , debug : Bool
    , check : Bool
    , hashKind : Hash.Kind
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
        (Input
         -> Hash Temporary
         -> BackendTask (Error e) ()
        )
    -> BuildTask e (Hash Normal)
derive description target inner =
    BuildTask "derive"
        (\({ existing, buildPath } as input_) state ->
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
            Do.do appendLog <| \_ ->
            if (HashSet.member target existing && not input_.check) || HashSet.member target state.deps then
                BackendTask.succeed ( target, { deps = newDeps, warnings = state.warnings } )

            else
                let
                    targetPath : String
                    targetPath =
                        Hash.toPath buildPath target
                in
                Do.do (File.exists targetPath) <| \exists ->
                if exists && not input_.check then
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
                    <| \_ ->
                    Do.do
                        (inner input_ tmp
                            |> BackendTask.onError
                                (\e ->
                                    Do.do
                                        (BackendTask.File.Extra.removeFileIfExists tmpPath
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
                                (Script.exec "diff" [ "-r", tmpPath, targetPath ]
                                    |> BackendTask.onError
                                        (\e ->
                                            Do.log ("Error inside " ++ description) <| \_ ->
                                            BackendTask.fail (InternalError e)
                                        )
                                )
                            <| \_ ->
                            BackendTask.File.Extra.removeFileIfExists tmpPath
                                |> BackendTask.mapError InternalError

                         else
                            Script.move { from = tmpPath, to = targetPath }
                                |> BackendTask.onError
                                    (\e ->
                                        Do.log ("Error inside " ++ description) <| \_ ->
                                        BackendTask.fail (InternalError e)
                                    )
                        )
                    <| \_ ->
                    Do.do
                        (Script.exec "chmod" [ "-R", "a=rX", targetPath ]
                            |> BackendTask.mapError InternalError
                        )
                    <| \_ ->
                    BackendTask.succeed ( target, { deps = newDeps, warnings = state.warnings } )
        )


runMonad : BuildTask e a -> Input -> State -> BackendTask (Error e) ( a, State )
runMonad (BuildTask label m) input_ state =
    if input_.debug then
        m input_ state
            |> BackendTask.andThen
                (\( v, newState ) ->
                    if HashSet.equals (HashSet.union state.deps newState.deps) newState.deps then
                        if Set.equals (Set.union state.warnings newState.warnings) newState.warnings then
                            BackendTask.succeed ( v, newState )

                        else
                            BackendTask.fail (InternalError (FatalError.fromString ("Missed warning inside " ++ label)))

                    else
                        BackendTask.fail (InternalError (FatalError.fromString ("Missed dependency inside " ++ label)))
                )

    else
        m input_ state


{-| -}
map : (a -> b) -> BuildTask e a -> BuildTask e b
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
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
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
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
    -> BuildTask e d
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
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
    -> BuildTask e d
    -> BuildTask e r
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
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
    -> BuildTask e d
    -> BuildTask e f
    -> BuildTask e g
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
andThen : (a -> BuildTask e b) -> BuildTask e a -> BuildTask e b
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
andThen2 : (a -> b -> BuildTask e c) -> BuildTask e a -> BuildTask e b -> BuildTask e c
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
    -> (String -> BuildTask { fatal : FatalError, recoverable : File.FileReadError decoderError } a)
    -> BuildTask { fatal : FatalError, recoverable : File.FileReadError decoderError } a
withFile hash f =
    BuildTask "withFile"
        (\({ buildPath } as input_) state ->
            Do.do
                (File.rawFile (Hash.toPath buildPath hash)
                    |> BackendTask.mapError UserError
                )
            <| \raw ->
            runMonad (f raw) input_ { deps = HashSet.insert hash state.deps, warnings = state.warnings }
        )


run : { jobs : Maybe Int, debug : Bool, check : Bool, hashKind : Hash.Kind } -> Path -> BuildTask e (Hash Normal) -> BackendTask (Error e) { output : Path, intermediate : List Path, warnings : Set String }
run config buildPath m =
    Do.do (listExisting buildPath |> BackendTask.mapError InternalError) <| \existing ->
    Do.do
        (case config.jobs of
            Nothing ->
                BackendTask.mapError InternalError nproc

            Just j ->
                BackendTask.succeed j
        )
    <| \jobs_ ->
    let
        input_ : Input
        input_ =
            { existing = existing
            , prefix = []
            , buildPath = buildPath
            , jobs = jobs_
            , debug = config.debug
            , hashKind = config.hashKind
            , check = config.check
            }
    in
    runMonad m input_ { deps = HashSet.empty, warnings = Set.empty }
        |> BackendTask.map
            (\( output, state ) ->
                { output = Path.path (Hash.toPath buildPath output)
                , intermediate =
                    state.deps
                        |> HashSet.toList
                        |> List.map (\raw -> raw |> Hash.toPath buildPath |> Path.path)
                , warnings = state.warnings
                }
            )


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


combineBy : Int -> List (BuildTask e a) -> BuildTask e (List a)
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


sequence : List (BuildTask e a) -> BuildTask e (List a)
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


timed : String -> String -> BuildTask e a -> BuildTask e a
timed before after task =
    BuildTask "timed" (\input_ deps -> BackendTask.Extra.timed before after (runMonad task input_ deps))


withPrefix : String -> BuildTask e a -> BuildTask e a
withPrefix newPrefix m =
    BuildTask "withPrefix" (\input_ deps -> runMonad m { input_ | prefix = newPrefix :: input_.prefix } deps)


jobs : BuildTask e Int
jobs =
    BuildTask "jobs" (\input_ deps -> BackendTask.succeed ( input_.jobs, deps ))


nproc : BackendTask FatalError Int
nproc =
    let
        tryRunningOrElse :
            String
            -> List String
            -> (() -> BackendTask FatalError Int)
            -> BackendTask FatalError Int
        tryRunningOrElse cmd args orElse =
            Do.do (Script.command cmd args |> BackendTask.toResult) <| \nprocResult ->
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
                                    "Invalid " ++ String.join " " (cmd :: args) ++ " output: " ++ Json.Encode.encode 0 (Json.Encode.string trimmed)
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


fail : msg -> BuildTask msg a
fail msg =
    triggerDebugger
        |> andThen
            (\_ ->
                BuildTask "fail"
                    (\{ prefix } _ ->
                        BackendTask.fail (UserError msg)
                     -- (FatalError.build
                     --     { title = String.join " " (prefix ++ [ "-", "build error" ])
                     --     , body = msg
                     --     }
                     -- )
                    )
            )


withWarning : Warning -> BuildTask e a -> BuildTask e a
withWarning warning (BuildTask n f) =
    BuildTask n
        (\input_ deps ->
            f input_ deps
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


input : Path -> BuildTask FatalError (Hash Normal)
input inputPath =
    BuildTask "input"
        (\{ prefix } deps ->
            Do.do
                (commandLog prefix "b3sum" [ Path.toString inputPath ]
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
                derive "input" hash <| \{ prefix, buildPath } target ->
                Script.copyFile { from = Path.toString inputPath, to = Hash.toPathTemporary buildPath target }
                    |> BackendTask.mapError UserError
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
        case Hex.fromString sha256 of
            Err _ ->
                fail (InvalidHashHex sha256)

            Ok hex ->
                derive "downloadSHA256" (Hash.build hex) <| \{ prefix, buildPath } target ->
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
                <| \_ ->
                Do.do
                    (commandLog prefix "sha256sum" [ tmpPath ]
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
commandLog : List String -> String -> List String -> BackendTask { fatal : FatalError, recoverable : Stream.Error Int String } String
commandLog prefix cmd args =
    Stream.command
        cmd
        args
        |> Stream.read
        |> BackendTask.map .body
        |> logCommand prefix cmd args


{-| -}
commandLogWith : CommandOptions -> List String -> String -> List String -> BackendTask { fatal : FatalError, recoverable : Stream.Error Int String } String
commandLogWith options prefix cmd args =
    Stream.commandWithOptions
        (CommandOptions.toStreamCommandOptions options)
        cmd
        args
        |> Stream.read
        |> BackendTask.map .body
        |> logCommand prefix cmd args


{-| -}
logCommand : List String -> String -> List String -> BackendTask error a -> BackendTask error a
logCommand prefix cmd args task =
    BackendTask.Extra.timed
        (String.join " " (prefix ++ "Running" :: cmd :: args))
        (String.join " " (prefix ++ "Ran    " :: cmd :: args))
        task


{-| -}
execLog : List String -> String -> List String -> BackendTask FatalError ()
execLog prefix cmd args =
    logCommand prefix cmd args (Script.exec cmd args)


named : String -> (a -> { files : List (Hash Normal), additionalData : List String }) -> (a -> BuildTask e (Hash Normal)) -> a -> BuildTask e (Hash Normal)
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
toResult (BuildTask name f) =
    BuildTask name
        (\input_ deps ->
            f input_ deps
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
        (\input_ state ->
            List.foldl (\( k, v ) a -> BackendTask.withEnv k v a)
                (f
                    input_
                    { state
                        | deps =
                            List.foldl
                                (\( k, v ) a ->
                                    HashSet.insert (Hash.fromString ("ENV" ++ k ++ "=" ++ v) input_.hashKind) a
                                )
                                state.deps
                                env
                    }
                )
                env
        )


mapError :
    (e -> f)
    -> BuildTask e a
    -> BuildTask f a
mapError f (BuildTask name t) =
    BuildTask name
        (\input_ state ->
            t input_ state
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
allowFatal (BuildTask name t) =
    BuildTask name
        (\input_ state ->
            t input_ state
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
fatalToInternal (BuildTask name t) =
    BuildTask name
        (\input_ state ->
            t input_ state
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
                derive ("extract " ++ file) outputHash <| \{ buildPath } target ->
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
