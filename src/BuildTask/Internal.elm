module BuildTask.Internal exposing (BuildTask(..), Hash, HashSet, Input, andThen, combineBy, commandLog, derive, execLog, extendHashWith, fail, hashToPath, hashToString, hashToWorkspace, input, inputHash, jobs, map, map2, map3, map4, named, run, sequence, stringToHash, succeed, timed, triggerDebugger, withFile, withPrefix)

import BST exposing (BST)
import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Stream as Stream
import FNV1a
import FatalError exposing (FatalError)
import Hex
import Json.Decode
import Json.Encode
import Pages.Script as Script
import Path exposing (Path)


type BuildTask a
    = BuildTask
        String
        (Input
         -> HashSet -- deps
         -> BackendTask FatalError ( a, HashSet )
        )


type alias Input =
    { existing : HashSet
    , prefix : List String
    , buildPath : Path
    , jobs : Maybe Int
    , debug : Bool
    }


derive :
    String
    -> Hash
    ->
        (Input
         -> Hash
         -> BackendTask FatalError ()
        )
    -> BuildTask Hash
derive description target inner =
    BuildTask "derive"
        (\({ existing, buildPath } as input_) deps ->
            let
                newDeps : HashSet
                newDeps =
                    hashSetInsert target deps

                appendLog : BackendTask FatalError ()
                appendLog =
                    if input_.debug then
                        (hashToPath buildPath target
                            ++ ": "
                            ++ description
                         -- ++ " from "
                         -- ++ (deps
                         --         |> hashSetToList
                         --         |> List.map (hashToPath buildPath)
                         --         |> String.join ", "
                         --    )
                        )
                            |> Script.log

                    else
                        BackendTask.succeed ()
            in
            Do.do appendLog <| \_ ->
            if hashSetMember target existing || hashSetMember target deps then
                BackendTask.succeed ( target, newDeps )

            else
                Do.do (fileExists (hashToPath buildPath target)) <| \exists ->
                if exists then
                    BackendTask.succeed ( target, newDeps )

                else
                    let
                        tmp : Hash
                        tmp =
                            hashToTmp target
                    in
                    Do.exec "rm" [ "-rf", hashToPath buildPath tmp ] <| \_ ->
                    Do.do
                        (inner input_ tmp
                            |> BackendTask.onError
                                (\e ->
                                    Do.exec "rm" [ "-rf", hashToPath buildPath tmp ] <| \_ ->
                                    BackendTask.fail e
                                )
                        )
                    <| \_ ->
                    Do.do
                        (Script.exec "mv" [ hashToPath buildPath tmp, hashToPath buildPath target ]
                            |> BackendTask.onError
                                (\e ->
                                    Do.do (fileExists (hashToPath buildPath target)) <| \exists2 ->
                                    Do.log ("Error inside " ++ description ++ ", exists " ++ hashToPath buildPath target ++ ": " ++ Debug.toString exists2) <| \_ ->
                                    BackendTask.fail e
                                )
                        )
                    <| \_ ->
                    Do.exec "chmod" [ "-R", "a=rX", hashToPath buildPath target ] <| \_ ->
                    BackendTask.succeed ( target, newDeps )
        )


fileExists : String -> BackendTask FatalError Bool
fileExists path =
    BackendTask.Custom.run "exists" (Json.Encode.string path) Json.Decode.bool
        |> BackendTask.allowFatal


runMonad : BuildTask a -> Input -> HashSet -> BackendTask FatalError ( a, HashSet )
runMonad (BuildTask label m) input_ deps =
    if input_.debug then
        m input_ deps
            |> BackendTask.andThen
                (\( v, newDeps ) ->
                    if hashSetToList (hashSetUnion deps newDeps) == hashSetToList newDeps then
                        BackendTask.succeed ( v, newDeps )

                    else
                        BackendTask.fail (FatalError.fromString ("Missed dependency inside " ++ label))
                )

    else
        m input_ deps


{-| -}
map : (a -> b) -> BuildTask a -> BuildTask b
map f m =
    BuildTask "map"
        (\input_ deps ->
            runMonad m input_ deps
                |> BackendTask.map
                    (\( v, newDeps ) ->
                        ( f v, newDeps )
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
                (\( va, depsA ) ( vb, depsB ) ->
                    ( f va vb, hashSetUnion depsA depsB )
                )
                (runMonad a input_ deps)
                (runMonad b input_ deps)
        )


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
                (\( va, depsA ) ( vb, depsB ) ( vc, depsC ) ->
                    ( f va vb vc, hashSetUnion depsA depsB |> hashSetUnion depsC )
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
                (\( va, depsA ) ( vb, depsB ) ( vc, depsC ) ( vd, depsD ) ->
                    ( f va vb vc vd
                    , hashSetUnion
                        (hashSetUnion depsA depsB)
                        (hashSetUnion depsC depsD)
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


withFile : Hash -> (String -> BuildTask a) -> BuildTask a
withFile hash f =
    BuildTask "withFile"
        (\({ buildPath } as input_) deps ->
            Do.allowFatal (File.rawFile (hashToPath buildPath hash)) <| \raw ->
            runMonad (f raw) input_ (hashSetInsert hash deps)
        )


run : { jobs : Maybe Int, debug : Bool } -> Path -> BuildTask Hash -> BackendTask FatalError { output : Path, intermediate : List Path }
run config buildPath m =
    Do.do (listExisting buildPath) <| \existing ->
    let
        input_ : Input
        input_ =
            { existing = existing
            , prefix = []
            , buildPath = buildPath
            , jobs = config.jobs
            , debug = config.debug
            }
    in
    runMonad m input_ hashSetEmpty
        |> BackendTask.map
            (\( output, deps ) ->
                { output = Path.path (hashToPath buildPath output)
                , intermediate =
                    deps
                        |> hashSetToList
                        |> List.map (\raw -> raw |> hashToPath buildPath |> Path.path)
                }
            )


listExisting : Path -> BackendTask FatalError HashSet
listExisting path =
    BackendTask.Custom.run "readdir"
        (Json.Encode.string (Path.toString path))
        (Json.Decode.list Json.Decode.string)
        |> BackendTask.allowFatal
        |> BackendTask.map hashSetFromList


combineBy : Int -> List (BuildTask a) -> BuildTask (List a)
combineBy n ops =
    BuildTask "combineBy"
        (\input_ deps ->
            case ops of
                [] ->
                    BackendTask.succeed ( [], deps )

                _ :: _ ->
                    ops
                        |> List.indexedMap
                            (\i m -> BackendTask.map (Tuple.pair i) (runMonad m input_ deps))
                        |> BackendTask.Extra.combineBy n
                        |> BackendTask.map
                            (\resList ->
                                resList
                                    |> List.sortBy (\( i, _ ) -> -i)
                                    |> List.foldl
                                        (\( _, ( res, newDeps ) ) ( resAcc, depsAcc ) ->
                                            ( res :: resAcc, hashSetUnion newDeps depsAcc )
                                        )
                                        ( [], hashSetEmpty )
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
                                |> Tuple.mapSecond (List.foldl hashSetUnion hashSetEmpty)
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
    BuildTask "jobs"
        (\input_ deps ->
            let
                task : BackendTask FatalError Int
                task =
                    case input_.jobs of
                        Just j ->
                            BackendTask.succeed j

                        Nothing ->
                            tryRunningOrElse "nproc" [ "--all" ] <| \_ ->
                            tryRunningOrElse "sysctl" [ "-n", "hw.logicalcpu" ] <| \_ ->
                            let
                                message : String
                                message =
                                    "Failed to run either `nproc --all` or `sysctl -n hw.logicalcpu`. You can work around this by specifying an explicit number of jobs to run in parallel"
                            in
                            BackendTask.fail (FatalError.fromString message)
            in
            task |> BackendTask.map (\j -> ( j, deps ))
        )


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


triggerDebugger : BuildTask ()
triggerDebugger =
    BuildTask "triggerDebugger"
        (\_ deps ->
            BackendTask.Custom.run "triggerDebugger" Json.Encode.null (Json.Decode.succeed ( (), deps ))
                |> BackendTask.allowFatal
        )


input : Path -> BuildTask Hash
input inputPath =
    BuildTask "input"
        (\{ prefix } deps ->
            Do.do (commandLog prefix "b3sum" [ Path.toString inputPath ]) <| \body ->
            case inputHash body of
                Err e ->
                    BackendTask.fail (FatalError.fromString e)

                Ok hash ->
                    BackendTask.succeed ( hash, deps )
        )
        |> andThen
            (\hash ->
                derive "input" hash <| \{ prefix, buildPath } target ->
                execLog prefix "cp" [ Path.toString inputPath, hashToPath buildPath target ]
            )


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



------------
-- HASHES --
------------


{-| -}
type Hash
    = Hash String


{-| Build an hashed file from the output of shaXsum/b3sum.
-}
inputHash : String -> Result String Hash
inputHash raw =
    let
        clean : String
        clean =
            String.left 8 raw
    in
    Hex.fromString clean
        |> Result.map (\_ -> Hash clean)


hashToPath : Path -> Hash -> String
hashToPath buildPath (Hash hash) =
    Path.toString buildPath ++ "/" ++ hash


hashToString : Hash -> String
hashToString (Hash hash) =
    hash


hashToTmp : Hash -> Hash
hashToTmp (Hash hash) =
    Hash ("tmp-" ++ hash)


hashToWorkspace : Hash -> Hash
hashToWorkspace (Hash hash) =
    Hash ("workspace-" ++ hash)


extendHashWith : List String -> Hash -> Hash
extendHashWith l (Hash r) =
    stringToHash (String.join "|" (r :: l))


stringToHash : String -> Hash
stringToHash raw =
    raw
        |> FNV1a.hash
        |> Hex.toString
        |> String.padLeft 8 '0'
        |> Hash


type HashSet
    = HashSet (BST String)


hashSetEmpty : HashSet
hashSetEmpty =
    HashSet BST.empty


hashSetMember : Hash -> HashSet -> Bool
hashSetMember (Hash x) (HashSet s) =
    BST.member x s


hashSetInsert : Hash -> HashSet -> HashSet
hashSetInsert (Hash x) (HashSet s) =
    HashSet (BST.insert x s)


hashSetUnion : HashSet -> HashSet -> HashSet
hashSetUnion (HashSet a) (HashSet b) =
    HashSet (BST.union a b)


hashSetToList : HashSet -> List Hash
hashSetToList (HashSet s) =
    BST.toList s
        |> List.map Hash


hashSetFromList : List String -> HashSet
hashSetFromList list =
    list
        |> BST.fromList
        |> HashSet


named : String -> (a -> { files : List Hash, additionalData : List String }) -> (a -> BuildTask Hash) -> a -> BuildTask Hash
named name encode action param =
    let
        encoded : { files : List Hash, additionalData : List String }
        encoded =
            encode param

        target : Hash
        target =
            (name :: encoded.additionalData ++ List.map hashToString encoded.files)
                |> String.join "|"
                |> stringToHash
    in
    BuildTask "named"
        (\input_ deps ->
            if hashSetMember target input_.existing || hashSetMember target deps then
                BackendTask.succeed ( target, hashSetInsert target deps )

            else
                runMonad (action param) input_ deps
                    |> BackendTask.andThen
                        (\( actionOutput, newDeps ) ->
                            Do.exec "mv"
                                [ hashToPath input_.buildPath actionOutput
                                , hashToPath input_.buildPath target
                                ]
                            <| \_ ->
                            BackendTask.succeed ( target, hashSetInsert target newDeps )
                        )
        )
