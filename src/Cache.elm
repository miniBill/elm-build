module Cache exposing
    ( FileOrDirectory, input, inputs
    , Monad, do, succeed, fail
    , writeFile, run
    , map, map2, map3, map4, andThen, combine, combineBy, each, sequence
    , pipeThrough, commandWithFile, commandInReadonlyDirectory, commandInWritableDirectory, withFile
    , withPrefix, timed
    , jobs, triggerDebugger
    )

{-|


## Input

@docs FileOrDirectory, input, inputs


## Building blocks

@docs Monad, do, succeed, fail


## Output

@docs writeFile, run


## Transforming and combining `Monad` values

@docs map, map2, map3, map4, andThen, combine, combineBy, each, sequence


## Operations

@docs pipeThrough, commandWithFile, commandInReadonlyDirectory, commandInWritableDirectory, withFile


## Output control

@docs withPrefix, timed


## Utils

@docs jobs, triggerDebugger

-}

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Stream as Stream
import Cache.Internal as Internal exposing (HashSet, Monad(..))
import FastDict as Dict exposing (Dict)
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import Pages.Script as Script
import Path exposing (Path)
import Result.Extra


{-| -}
type alias Monad a =
    Internal.Monad a


type alias Input =
    { existing : HashSet
    , prefix : List String
    , buildPath : Path
    , jobs : Maybe Int
    }


type alias FileOrDirectory =
    Internal.Hash


{-| -}
succeed : a -> Monad a
succeed v =
    Monad "succeed" (\_ deps -> BackendTask.succeed ( v, deps ))


debug : Bool
debug =
    False


runMonad : Monad a -> Input -> HashSet -> BackendTask FatalError ( a, HashSet )
runMonad (Monad label m) input_ deps =
    if debug then
        m input_ deps
            |> BackendTask.andThen
                (\( v, newDeps ) ->
                    if Internal.hashSetToList (Internal.hashSetUnion deps newDeps) == Internal.hashSetToList newDeps then
                        BackendTask.succeed ( v, newDeps )

                    else
                        BackendTask.fail (FatalError.fromString ("Missed dependency inside " ++ label))
                )

    else
        m input_ deps


{-| -}
map : (a -> b) -> Monad a -> Monad b
map f m =
    Monad "map"
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
    -> Monad a
    -> Monad b
    -> Monad c
map2 f a b =
    Monad "map2"
        (\input_ deps ->
            BackendTask.map2
                (\( va, depsA ) ( vb, depsB ) ->
                    ( f va vb, Internal.hashSetUnion depsA depsB )
                )
                (runMonad a input_ deps)
                (runMonad b input_ deps)
        )


{-| -}
map3 :
    (a -> b -> c -> d)
    -> Monad a
    -> Monad b
    -> Monad c
    -> Monad d
map3 f a b c =
    Monad "map3"
        (\input_ deps ->
            BackendTask.map3
                (\( va, depsA ) ( vb, depsB ) ( vc, depsC ) ->
                    ( f va vb vc, Internal.hashSetUnion depsA depsB |> Internal.hashSetUnion depsC )
                )
                (runMonad a input_ deps)
                (runMonad b input_ deps)
                (runMonad c input_ deps)
        )


{-| -}
map4 :
    (a -> b -> c -> d -> e)
    -> Monad a
    -> Monad b
    -> Monad c
    -> Monad d
    -> Monad e
map4 f a b c d =
    Monad "map4"
        (\input_ deps ->
            BackendTask.map4
                (\( va, depsA ) ( vb, depsB ) ( vc, depsC ) ( vd, depsD ) ->
                    ( f va vb vc vd
                    , Internal.hashSetUnion
                        (Internal.hashSetUnion depsA depsB)
                        (Internal.hashSetUnion depsC depsD)
                    )
                )
                (runMonad a input_ deps)
                (runMonad b input_ deps)
                (runMonad c input_ deps)
                (runMonad d input_ deps)
        )


{-| -}
andThen : (a -> Monad b) -> Monad a -> Monad b
andThen f m =
    Monad "andThen"
        (\input_ deps ->
            runMonad m input_ deps
                |> BackendTask.andThen
                    (\( v, newDeps ) ->
                        runMonad (f v) input_ newDeps
                    )
        )


{-| -}
fail : String -> Monad a
fail msg =
    do triggerDebugger <| \_ ->
    Monad "fail" (\_ _ -> BackendTask.fail (FatalError.fromString msg))


{-| -}
triggerDebugger : Monad ()
triggerDebugger =
    Monad "triggerDebugger"
        (\_ deps ->
            BackendTask.Custom.run "triggerDebugger" Json.Encode.null (Json.Decode.succeed ( (), deps ))
                |> BackendTask.allowFatal
        )


{-| -}
input : Path -> (FileOrDirectory -> Monad a) -> Monad a
input inputPath k =
    Monad "input"
        (\{ prefix } deps ->
            Do.do (commandLog prefix "b3sum" [ Path.toString inputPath ]) <| \body ->
            case Internal.inputHash body of
                Err e ->
                    BackendTask.fail (FatalError.fromString e)

                Ok hash ->
                    BackendTask.succeed ( hash, deps )
        )
        |> andThen
            (\hash ->
                Internal.derive {- "input" -} hash <| \{ prefix, buildPath } target ->
                execLog prefix "cp" [ Path.toString inputPath, Internal.hashToPath buildPath target ]
            )
        |> andThen k


{-| -}
inputs :
    List Path
    ->
        BackendTask
            FatalError
            (List
                ( Path
                , Monad FileOrDirectory
                )
            )
inputs inputPaths =
    Do.do (commandLog [] "b3sum" (List.map Path.toString inputPaths)) <| \body ->
    List.map2
        (\inputPath line ->
            case Internal.inputHash line of
                Ok hash ->
                    ( inputPath
                    , Internal.derive {- "inputs" -} hash <| \{ prefix, buildPath } target ->
                    execLog prefix "cp" [ Path.toString inputPath, Internal.hashToPath buildPath target ]
                    )
                        |> Ok

                Err e ->
                    Err (FatalError.fromString e)
        )
        inputPaths
        (String.lines body)
        |> Result.Extra.combine
        |> BackendTask.fromResult


type Tree
    = Tree
        { files : Dict String FileOrDirectory
        , directories : Dict String Tree
        }


{-| -}
combine : List { filename : Path, hash : FileOrDirectory } -> Monad FileOrDirectory
combine files =
    files
        |> buildTree
        |> combineTree


buildTree : List { filename : Path, hash : FileOrDirectory } -> Tree
buildTree files =
    let
        emptyTree : Tree
        emptyTree =
            Tree { files = Dict.empty, directories = Dict.empty }

        addFile : { filename : Path, hash : FileOrDirectory } -> Tree -> Tree
        addFile file tree =
            let
                dir : List String
                dir =
                    file.filename
                        |> Path.directory
                        |> Path.toString
                        |> String.split "/"

                filename : String
                filename =
                    Path.filename file.filename
            in
            addFile_ dir filename file.hash tree

        addFile_ : List String -> String -> FileOrDirectory -> Tree -> Tree
        addFile_ dir filename hash (Tree tree) =
            case dir of
                "" :: tail ->
                    addFile_ tail filename hash (Tree tree)

                [] ->
                    Tree
                        { files = Dict.insert filename hash tree.files
                        , directories = tree.directories
                        }

                head :: tail ->
                    Tree
                        { files = tree.files
                        , directories =
                            Dict.update head
                                (\found ->
                                    found
                                        |> Maybe.withDefault emptyTree
                                        |> addFile_ tail filename hash
                                        |> Just
                                )
                                tree.directories
                        }
    in
    List.foldl addFile emptyTree files


combineTree : Tree -> Monad FileOrDirectory
combineTree (Tree tree) =
    do jobs <| \parallelism ->
    do
        (tree.directories
            |> Dict.foldl (\filename subtree acc -> map (Tuple.pair filename) (combineTree subtree) :: acc) []
            |> combineBy parallelism
            |> map Dict.fromList
        )
    <| \subtrees ->
    let
        combined : Dict String FileOrDirectory
        combined =
            Dict.union tree.files subtrees

        outputHash : FileOrDirectory
        outputHash =
            Dict.foldl
                (\filename hash acc -> filename :: Internal.hashToString hash :: acc)
                [ "combineTree" ]
                combined
                |> String.join "|"
                |> Internal.stringToHash
    in
    Internal.derive {- "combine" -} outputHash <| \{ prefix, buildPath } target ->
    Do.do (execLog prefix "mkdir" [ "-p", Internal.hashToPath buildPath target ]) <| \_ ->
    combined
        |> Dict.foldl (\outputFilename hash acc -> execLog prefix "cp" [ "-rl", Internal.hashToPath buildPath hash, Internal.hashToPath buildPath target ++ "/" ++ outputFilename ] :: acc) []
        |> BackendTask.Extra.combineBy_ parallelism


{-| -}
pipeThrough : String -> List String -> FileOrDirectory -> Monad FileOrDirectory
pipeThrough cmd args hash =
    let
        outputHash : FileOrDirectory
        outputHash =
            Internal.extendHashWith (cmd :: args) hash
    in
    Internal.derive {- (String.join " " ("pipeThrough" :: cmd :: args)) -} outputHash <| \{ prefix, buildPath } target ->
    BackendTask.Extra.timed
        (String.join " " (prefix ++ "Piping" :: Internal.hashToPath buildPath hash :: "through" :: cmd :: args))
        (String.join " " (prefix ++ "Piped " :: Internal.hashToPath buildPath hash :: "through" :: cmd :: args))
        (Stream.fileRead (Internal.hashToPath buildPath hash)
            |> Stream.pipe (Stream.command cmd args)
            |> Stream.pipe (Stream.fileWrite (Internal.hashToPath buildPath target))
            |> Stream.run
        )


{-| Run a command in a specific generated directory and save the result to a file.

To avoid needing to copy files, the directory will be read-only. If you need a writable directory use `commandInWritableDirectory` instead.

This models commands as pure functions: given the same directory contents and
arguments, the command produces the same stdout. The temporary directory handles the
implementation detail that many tools need to write intermediate files.

**IMPORTANT**: The command should only read files from that directory,
otherwise elm-build has no way to know when to re-run it.

-}
commandInReadonlyDirectory : String -> List String -> FileOrDirectory -> Monad FileOrDirectory
commandInReadonlyDirectory cmd args hash =
    let
        outputHash : FileOrDirectory
        outputHash =
            Internal.extendHashWith (cmd :: args) hash
    in
    Internal.derive {- (String.join " " ("commandInReadonlyDirectory" :: cmd :: args)) -} outputHash <| \{ prefix, buildPath } target ->
    Do.do (commandLog prefix cmd args |> BackendTask.inDir (Internal.hashToPath buildPath hash)) <| \output ->
    BackendTask.allowFatal (Script.writeFile { path = Internal.hashToPath buildPath target, body = output })


{-| Run a command in a writable temporary directory seeded from a cached directory.

Unlike `commandInReadonlyDirectory` (which runs in the read-only cached directory directly),
this creates a writable copy so the command can create temporary files
(like `elm-stuff/` during compilation). Only stdout is captured and cached;
the temporary directory is discarded after the command completes.

This models commands as pure functions: given the same directory contents and
arguments, the command produces the same stdout. The temporary directory handles the
implementation detail that many tools need to write intermediate files.

**IMPORTANT**: The command should only read files from the temporary directory,
otherwise elm-build has no way to know when to re-run it.

-}
commandInWritableDirectory : String -> List String -> FileOrDirectory -> Monad FileOrDirectory
commandInWritableDirectory cmd args hash =
    let
        outputHash : FileOrDirectory
        outputHash =
            Internal.extendHashWith (cmd :: args) hash
    in
    Internal.derive outputHash <| \{ prefix, buildPath } target ->
    let
        workspacePath : String
        workspacePath =
            Internal.hashToPath buildPath (Internal.hashToWorkspace target)
    in
    Do.exec "rm" [ "-rf", workspacePath ] <| \_ ->
    Do.exec "cp" [ "-r", Internal.hashToPath buildPath hash, workspacePath ] <| \_ ->
    Do.exec "chmod" [ "-R", "u+w", workspacePath ] <| \_ ->
    Do.do
        (commandLog prefix cmd args
            |> BackendTask.inDir workspacePath
            |> BackendTask.Extra.finally
                (Script.exec "rm" [ "-rf", workspacePath ])
        )
    <| \output ->
    BackendTask.allowFatal (Script.writeFile { path = Internal.hashToPath buildPath target, body = output })


{-| Run a command passing in a file (or directory) as last argument and save the result to a file.

**IMPORTANT**: The command should only read that file or directory, otherwise elm-build has no way to know when to re-run it.

-}
commandWithFile :
    String
    -> List String
    -> FileOrDirectory
    -> Monad FileOrDirectory
commandWithFile cmd args hash =
    let
        outputHash : FileOrDirectory
        outputHash =
            Internal.extendHashWith (cmd :: args) hash
    in
    Internal.derive {- (String.join " " ("commandWithFile" :: cmd :: args)) -} outputHash <| \{ prefix, buildPath } target ->
    Do.do (commandLog prefix cmd (args ++ [ Internal.hashToPath buildPath hash ])) <| \output ->
    BackendTask.allowFatal (Script.writeFile { path = Internal.hashToPath buildPath target, body = output })


{-| -}
execLog : List String -> String -> List String -> BackendTask FatalError ()
execLog prefix cmd args =
    logCommand prefix cmd args (Script.exec cmd args)


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
do : Monad b -> (b -> Monad a) -> Monad a
do x f =
    andThen f x


{-| -}
withFile : FileOrDirectory -> (String -> Monad a) -> Monad a
withFile hash f =
    Monad "withFile"
        (\({ buildPath } as input_) deps ->
            Do.allowFatal (File.rawFile (Internal.hashToPath buildPath hash)) <| \raw ->
            runMonad (f raw) input_ (Internal.hashSetInsert hash deps)
        )


{-| -}
writeFile : String -> Monad FileOrDirectory
writeFile content =
    let
        hash : FileOrDirectory
        hash =
            Internal.stringToHash content
    in
    Internal.derive {- "writeFile" -} hash <| \{ buildPath } target ->
    BackendTask.allowFatal (Script.writeFile { path = Internal.hashToPath buildPath target, body = content })


{-| -}
run : { jobs : Maybe Int } -> Path -> Monad FileOrDirectory -> BackendTask FatalError { output : Path, dependencies : List Path }
run config buildPath m =
    Do.do (listExisting buildPath) <| \existing ->
    let
        input_ : Input
        input_ =
            { existing = existing
            , prefix = []
            , buildPath = buildPath
            , jobs = config.jobs
            }
    in
    runMonad m input_ Internal.hashSetEmpty
        |> BackendTask.map
            (\( output, deps ) ->
                { output = Path.path (Internal.hashToPath buildPath output)
                , dependencies =
                    deps
                        |> Internal.hashSetToList
                        |> List.map (\raw -> raw |> Internal.hashToPath buildPath |> Path.path)
                }
            )


listExisting : Path -> BackendTask FatalError HashSet
listExisting path =
    BackendTask.Custom.run "readdir"
        (Json.Encode.string (Path.toString path))
        (Json.Decode.list Json.Decode.string)
        |> BackendTask.allowFatal
        |> BackendTask.map Internal.hashSetFromList


{-| -}
combineBy : Int -> List (Monad a) -> Monad (List a)
combineBy n ops =
    Monad "combineBy"
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
                                            ( res :: resAcc, Internal.hashSetUnion newDeps depsAcc )
                                        )
                                        ( [], Internal.hashSetEmpty )
                            )
        )


{-| -}
each : List a -> (a -> Monad b) -> Monad (List b)
each l f =
    l |> List.map f |> sequence


{-| -}
sequence : List (Monad a) -> Monad (List a)
sequence ops =
    Monad "sequence"
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
                                |> Tuple.mapSecond (List.foldl Internal.hashSetUnion Internal.hashSetEmpty)
                        )
        )


{-| -}
timed : String -> String -> Monad a -> Monad a
timed before after task =
    Monad "timed" (\input_ deps -> BackendTask.Extra.timed before after (runMonad task input_ deps))


{-| -}
withPrefix : String -> Monad a -> Monad a
withPrefix newPrefix m =
    Monad "withPrefix" (\input_ deps -> runMonad m { input_ | prefix = newPrefix :: input_.prefix } deps)


jobs : Monad Int
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
    Monad "jobs"
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
