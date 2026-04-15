module BuildTask.Internal exposing (BuildTask(..), Input, Warning, andThen, andThen2, combineBy, commandLog, derive, downloadSHA256, execLog, extendHashWith, fail, hashFromString, input, jobs, map, map2, map3, map4, named, run, sequence, succeed, timed, toResult, triggerDebugger, withFile, withPrefix, withWarning)

import BackendTask exposing (BackendTask)
import BackendTask.Customs
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Http as Http
import BackendTask.Stream as Stream
import FastSet as Set exposing (Set)
import FatalError exposing (FatalError)
import Hash exposing (Hash, Normal, Temporary)
import HashSet exposing (HashSet)
import Hex
import Json.Encode
import Pages.Script as Script
import Parser.Error exposing (Output)
import Path exposing (Path)


type BuildTask a
    = BuildTask String (Input -> State -> BackendTask FatalError ( a, State ))


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
    , hashKind : Hash.Kind
    }


derive :
    String
    -> Hash Normal
    ->
        (Input
         -> Hash Temporary
         -> BackendTask FatalError ()
        )
    -> BuildTask (Hash Normal)
derive description target inner =
    BuildTask "derive"
        (\({ existing, buildPath } as input_) state ->
            let
                newDeps : HashSet
                newDeps =
                    HashSet.insert target state.deps

                appendLog : BackendTask FatalError ()
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
            if HashSet.member target existing || HashSet.member target state.deps then
                BackendTask.succeed ( target, { deps = newDeps, warnings = state.warnings } )

            else
                Do.do (File.exists (Hash.toPath buildPath target)) <| \exists ->
                if exists then
                    BackendTask.succeed ( target, { deps = newDeps, warnings = state.warnings } )

                else
                    let
                        tmp : Hash Temporary
                        tmp =
                            Hash.toTemporary target
                    in
                    Do.exec "rm" [ "-rf", Hash.toPathTemporary buildPath tmp ] <| \_ ->
                    Do.do
                        (inner input_ tmp
                            |> BackendTask.onError
                                (\e ->
                                    Do.exec "rm" [ "-rf", Hash.toPathTemporary buildPath tmp ] <| \_ ->
                                    BackendTask.fail e
                                )
                        )
                    <| \_ ->
                    Do.do
                        (Script.exec "mv" [ Hash.toPathTemporary buildPath tmp, Hash.toPath buildPath target ]
                            |> BackendTask.onError
                                (\e ->
                                    Do.log ("Error inside " ++ description) <| \_ ->
                                    BackendTask.fail e
                                )
                        )
                    <| \_ ->
                    Do.exec "chmod" [ "-R", "a=rX", Hash.toPath buildPath target ] <| \_ ->
                    BackendTask.succeed ( target, { deps = newDeps, warnings = state.warnings } )
        )


runMonad : BuildTask a -> Input -> State -> BackendTask FatalError ( a, State )
runMonad (BuildTask label m) input_ state =
    if input_.debug then
        m input_ state
            |> BackendTask.andThen
                (\( v, newState ) ->
                    if HashSet.equals (HashSet.union state.deps newState.deps) newState.deps then
                        if Set.equals (Set.union state.warnings newState.warnings) newState.warnings then
                            BackendTask.succeed ( v, newState )

                        else
                            BackendTask.fail (FatalError.fromString ("Missed warning inside " ++ label))

                    else
                        BackendTask.fail (FatalError.fromString ("Missed dependency inside " ++ label))
                )

    else
        m input_ state


{-| -}
map : (a -> b) -> BuildTask a -> BuildTask b
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
    -> BuildTask a
    -> BuildTask b
    -> BuildTask c
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
    -> BuildTask a
    -> BuildTask b
    -> BuildTask c
    -> BuildTask d
map3 f a b c =
    BuildTask "map3"
        (\input_ deps ->
            BackendTask.map3
                (\( va, outputA ) ( vb, outputB ) ( vc, outputC ) ->
                    ( f va vb vc, combineOutput outputA outputB |> combineOutput outputC )
                )
                (runMonad a input_ deps)
                (runMonad b input_ deps)
                (runMonad c input_ deps)
        )


{-| -}
map4 :
    (a -> b -> c -> d -> e)
    -> BuildTask a
    -> BuildTask b
    -> BuildTask c
    -> BuildTask d
    -> BuildTask e
map4 f a b c d =
    BuildTask "map4"
        (\input_ deps ->
            BackendTask.map4
                (\( va, outputA ) ( vb, outputB ) ( vc, outputC ) ( vd, outputD ) ->
                    ( f va vb vc vd
                    , combineOutput
                        (combineOutput outputA outputB)
                        (combineOutput outputC outputD)
                    )
                )
                (runMonad a input_ deps)
                (runMonad b input_ deps)
                (runMonad c input_ deps)
                (runMonad d input_ deps)
        )


{-| -}
andThen : (a -> BuildTask b) -> BuildTask a -> BuildTask b
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
andThen2 : (a -> b -> BuildTask c) -> BuildTask a -> BuildTask b -> BuildTask c
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


withFile : Hash Normal -> (String -> BuildTask a) -> BuildTask a
withFile hash f =
    BuildTask "withFile"
        (\({ buildPath } as input_) state ->
            Do.allowFatal (File.rawFile (Hash.toPath buildPath hash)) <| \raw ->
            runMonad (f raw) input_ { deps = HashSet.insert hash state.deps, warnings = state.warnings }
        )


run : { jobs : Maybe Int, debug : Bool, hashKind : Hash.Kind } -> Path -> BuildTask (Hash Normal) -> BackendTask FatalError { output : Path, intermediate : List Path, warnings : Set String }
run config buildPath m =
    Do.do (listExisting buildPath) <| \existing ->
    Do.do
        (case config.jobs of
            Nothing ->
                nproc

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
                case HashSet.fromList list of
                    Ok o ->
                        BackendTask.succeed o

                    Err e ->
                        BackendTask.fail (FatalError.fromString e)
            )


combineBy : Int -> List (BuildTask a) -> BuildTask (List a)
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


sequence : List (BuildTask a) -> BuildTask (List a)
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


timed : String -> String -> BuildTask a -> BuildTask a
timed before after task =
    BuildTask "timed" (\input_ deps -> BackendTask.Extra.timed before after (runMonad task input_ deps))


withPrefix : String -> BuildTask a -> BuildTask a
withPrefix newPrefix m =
    BuildTask "withPrefix" (\input_ deps -> runMonad m { input_ | prefix = newPrefix :: input_.prefix } deps)


jobs : BuildTask Int
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


succeed : a -> BuildTask a
succeed v =
    BuildTask "succeed" (\_ deps -> BackendTask.succeed ( v, deps ))


fail : String -> BuildTask a
fail msg =
    triggerDebugger
        |> andThen
            (\_ ->
                BuildTask "fail"
                    (\_ _ -> BackendTask.fail (FatalError.fromString msg))
            )


withWarning : Warning -> BuildTask a -> BuildTask a
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


triggerDebugger : BuildTask ()
triggerDebugger =
    BuildTask "triggerDebugger"
        (\_ deps ->
            BackendTask.Customs.triggerDebugger
                |> BackendTask.map (\() -> ( (), deps ))
        )


input : Path -> BuildTask (Hash Normal)
input inputPath =
    BuildTask "input"
        (\{ prefix } deps ->
            Do.do (commandLog prefix "b3sum" [ Path.toString inputPath ]) <| \body ->
            case Hash.fromChecksum body of
                Err e ->
                    BackendTask.fail (FatalError.fromString e)

                Ok hash ->
                    BackendTask.succeed ( hash, deps )
        )
        |> andThen
            (\hash ->
                derive "input" hash <| \{ prefix, buildPath } target ->
                execLog prefix "cp" [ Path.toString inputPath, Hash.toPathTemporary buildPath target ]
            )


downloadSHA256 : { url : String, sha256 : String } -> BuildTask (Hash Normal)
downloadSHA256 { url, sha256 } =
    let
        hashLength : Int
        hashLength =
            String.length sha256
    in
    if hashLength /= 64 then
        fail ("Wrong hash length: " ++ String.fromInt hashLength ++ " for hash " ++ sha256)

    else
        case Hex.fromString sha256 of
            Err e ->
                fail ("Invalid hex hash: " ++ e ++ " for hash " ++ sha256)

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
                    )
                <| \_ ->
                Do.do (commandLog prefix "sha256sum" [ tmpPath ]) <| \body ->
                if String.startsWith sha256 body then
                    BackendTask.succeed ()

                else
                    let
                        msg : String
                        msg =
                            "Wrong hash: expected " ++ sha256 ++ ", got " ++ String.left 64 body
                    in
                    BackendTask.fail (FatalError.fromString msg)


{-| -}
commandLog : List String -> String -> List String -> BackendTask FatalError String
commandLog prefix cmd args =
    logCommand prefix
        cmd
        args
        (Stream.commandWithOptions
            (Stream.defaultCommandOptions |> Stream.withOutput Stream.PrintStderr)
            cmd
            args
            |> Stream.read
            |> BackendTask.map .body
            |> BackendTask.allowFatal
        )


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


named : String -> (a -> { files : List (Hash Normal), additionalData : List String }) -> (a -> BuildTask (Hash Normal)) -> a -> BuildTask (Hash Normal)
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
                                        Do.exec "mv"
                                            [ Hash.toPath input_.buildPath actionOutput
                                            , Hash.toPath input_.buildPath hash
                                            ]
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


extendHashWith : List String -> Hash Normal -> BuildTask (Hash Normal)
extendHashWith l r =
    hashFromString (String.join "|" (Hash.toString r :: l))


hashFromString : String -> BuildTask (Hash Normal)
hashFromString raw =
    BuildTask "fromString"
        (\{ hashKind } deps ->
            BackendTask.succeed ( Hash.fromString raw hashKind, deps )
        )


toResult : BuildTask a -> BuildTask (Result FatalError a)
toResult (BuildTask name f) =
    BuildTask name
        (\input_ deps ->
            f input_ deps
                |> BackendTask.toResult
                |> BackendTask.map
                    (\res ->
                        case res of
                            Ok ( o, newDeps ) ->
                                ( Ok o, newDeps )

                            Err e ->
                                ( Err e, deps )
                    )
        )
