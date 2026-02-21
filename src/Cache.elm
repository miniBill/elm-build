module Cache exposing
    ( FileOrDirectory, input, inputs
    , Monad, do, succeed, fail
    , writeFile, run
    , map, map2, andThen, combine, combineBy, each, sequence
    , pipeThrough, commandWithFile, commandInReadonlyDirectory, commandInWritableDirectory, withFile
    , withPrefix, timed
    , jobs, triggerDebugger
    , map3
    )

{-|


## Input

@docs FileOrDirectory, input, inputs


## Building blocks

@docs Monad, do, succeed, fail


## Output

@docs writeFile, run


## Transforming and combining `Monad` values

@docs map, map2, andThen, combine, combineBy, each, sequence


## Operations

@docs pipeThrough, commandWithFile, commandInReadonlyDirectory, commandInWritableDirectory, withFile


## Output control

@docs withPrefix, timed


## Utils

@docs jobs, triggerDebugger

-}

import Array exposing (Array)
import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Stream as Stream
import FNV1a
import FastDict as Dict exposing (Dict)
import FatalError exposing (FatalError)
import Hex
import Json.Decode
import Json.Encode
import Pages.Script as Script
import Path exposing (Path)
import Result.Extra


{-| -}
type Monad a
    = Monad
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
    }


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
                    if hashSetToList (hashSetUnion deps newDeps) == hashSetToList newDeps then
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
map2 : (a -> b -> c) -> ((a -> Monad a) -> Monad a) -> ((b -> Monad b) -> Monad b) -> (c -> Monad d) -> Monad d
map2 f a b k =
    Monad "map2"
        (\input_ deps ->
            BackendTask.map2
                (\( va, depsA ) ( vb, depsB ) ->
                    ( f va vb, hashSetUnion depsA depsB )
                )
                (runMonad (a succeed) input_ deps)
                (runMonad (b succeed) input_ deps)
        )
        |> andThen k


{-| -}
map3 :
    (a -> b -> c -> d)
    -> ((a -> Monad a) -> Monad a)
    -> ((b -> Monad b) -> Monad b)
    -> ((c -> Monad c) -> Monad c)
    -> (d -> Monad e)
    -> Monad e
map3 f a b c k =
    Monad "map3"
        (\input_ deps ->
            BackendTask.map3
                (\( va, depsA ) ( vb, depsB ) ( vc, depsC ) ->
                    ( f va vb vc, hashSetUnion depsA depsB |> hashSetUnion depsC )
                )
                (runMonad (a succeed) input_ deps)
                (runMonad (b succeed) input_ deps)
                (runMonad (c succeed) input_ deps)
        )
        |> andThen k


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
    triggerDebugger <| \_ ->
    Monad "fail" (\_ _ -> BackendTask.fail (FatalError.fromString msg))


{-| -}
triggerDebugger : (() -> Monad a) -> Monad a
triggerDebugger k =
    Monad "triggerDebugger"
        (\_ deps ->
            BackendTask.Custom.run "triggerDebugger" Json.Encode.null (Json.Decode.succeed ( (), deps ))
                |> BackendTask.allowFatal
        )
        |> andThen k


{-| -}
input : Path -> (FileOrDirectory -> Monad a) -> Monad a
input inputPath k =
    Monad "input"
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
                derive {- "input" -} hash <| \{ prefix, buildPath } target ->
                execLog prefix "cp" [ Path.toString inputPath, hashToPath buildPath target ]
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
            case inputHash line of
                Ok hash ->
                    ( inputPath
                    , derive {- "inputs" -} hash <| \{ prefix, buildPath } target ->
                    execLog prefix "cp" [ Path.toString inputPath, hashToPath buildPath target ]
                    )
                        |> Ok

                Err e ->
                    Err (FatalError.fromString e)
        )
        inputPaths
        (String.lines body)
        |> Result.Extra.combine
        |> BackendTask.fromResult


derive :
    -- String ->
    FileOrDirectory
    ->
        (Input
         -> FileOrDirectory
         -> BackendTask FatalError ()
        )
    -> Monad FileOrDirectory
derive {- description -} target inner =
    Monad "derive"
        (\({ existing, buildPath } as input_) deps ->
            let
                newDeps : HashSet
                newDeps =
                    hashSetInsert target deps
            in
            -- Do.do
            --     (appendLog
            --         (hashToPath path   target
            --             ++ ": "
            --             ++ description
            --             ++ " from "
            --             ++ (prev
            --                     |> Set.toList
            --                     |> String.join ", "
            --                )
            --         )
            --         |> BackendTask.quiet
            --     )
            -- <| \_ ->
            if hashSetMember target existing || hashSetMember target deps then
                BackendTask.succeed ( target, newDeps )

            else
                let
                    tmp : FileOrDirectory
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
                Do.exec "mv" [ hashToPath buildPath tmp, hashToPath buildPath target ] <| \_ ->
                Do.exec "chmod" [ "-R", "a=rX", hashToPath buildPath target ] <| \_ ->
                BackendTask.succeed ( target, newDeps )
        )



-- appendLog : String -> BackendTask FatalError ()
-- appendLog line =
--     BackendTask.Custom.run "appendLog" (Json.Encode.string line) (Json.Decode.succeed ())
--         |> BackendTask.allowFatal


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
    jobs <| \parallelism ->
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
                (\filename hash acc -> filename :: hashToString hash :: acc)
                [ "combineTree" ]
                combined
                |> String.join "|"
                |> stringToHash
    in
    derive {- "combine" -} outputHash <| \{ prefix, buildPath } target ->
    Do.do (execLog prefix "mkdir" [ "-p", hashToPath buildPath target ]) <| \_ ->
    combined
        |> Dict.foldl (\outputFilename hash acc -> execLog prefix "cp" [ "-rl", hashToPath buildPath hash, hashToPath buildPath target ++ "/" ++ outputFilename ] :: acc) []
        |> BackendTask.Extra.combineBy_ parallelism


{-| -}
pipeThrough : String -> List String -> FileOrDirectory -> (FileOrDirectory -> Monad a) -> Monad a
pipeThrough cmd args hash k =
    let
        outputHash : FileOrDirectory
        outputHash =
            extendHashWith (cmd :: args) hash
    in
    (derive {- (String.join " " ("pipeThrough" :: cmd :: args)) -} outputHash <| \{ prefix, buildPath } target ->
    BackendTask.Extra.timed
        (String.join " " (prefix ++ "Piping" :: hashToPath buildPath hash :: "through" :: cmd :: args))
        (String.join " " (prefix ++ "Piped " :: hashToPath buildPath hash :: "through" :: cmd :: args))
        (Stream.fileRead (hashToPath buildPath hash)
            |> Stream.pipe (Stream.command cmd args)
            |> Stream.pipe (Stream.fileWrite (hashToPath buildPath target))
            |> Stream.run
        )
    )
        |> andThen k


{-| Run a command in a specific generated directory and save the result to a file.

To avoid needing to copy files, the directory will be read-only. If you need a writable directory use `commandInWritableDirectory` instead.

This models commands as pure functions: given the same directory contents and
arguments, the command produces the same stdout. The temporary directory handles the
implementation detail that many tools need to write intermediate files.

**IMPORTANT**: The command should only read files from that directory,
otherwise elm-build has no way to know when to re-run it.

-}
commandInReadonlyDirectory : String -> List String -> FileOrDirectory -> (FileOrDirectory -> Monad a) -> Monad a
commandInReadonlyDirectory cmd args hash k =
    let
        outputHash : FileOrDirectory
        outputHash =
            extendHashWith (cmd :: args) hash
    in
    (derive {- (String.join " " ("commandInReadonlyDirectory" :: cmd :: args)) -} outputHash <| \{ prefix, buildPath } target ->
    Do.do (commandLog prefix cmd args |> BackendTask.inDir (hashToPath buildPath hash)) <| \output ->
    BackendTask.allowFatal (Script.writeFile { path = hashToPath buildPath target, body = output })
    )
        |> andThen k


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
commandInWritableDirectory : String -> List String -> FileOrDirectory -> (FileOrDirectory -> Monad a) -> Monad a
commandInWritableDirectory cmd args hash k =
    let
        outputHash : FileOrDirectory
        outputHash =
            extendHashWith (cmd :: args) hash
    in
    (derive outputHash <| \{ prefix, buildPath } target ->
    let
        workspacePath : String
        workspacePath =
            hashToPath buildPath (hashToWorkspace target)
    in
    Do.exec "rm" [ "-rf", workspacePath ] <| \_ ->
    Do.exec "cp" [ "-r", hashToPath buildPath hash, workspacePath ] <| \_ ->
    Do.exec "chmod" [ "-R", "u+w", workspacePath ] <| \_ ->
    Do.do
        (commandLog prefix cmd args
            |> BackendTask.inDir workspacePath
            |> BackendTask.Extra.finally
                (Script.exec "rm" [ "-rf", workspacePath ])
        )
    <| \output ->
    BackendTask.allowFatal (Script.writeFile { path = hashToPath buildPath target, body = output })
    )
        |> andThen k


{-| Run a command passing in a file (or directory) as last argument and save the result to a file.

**IMPORTANT**: The command should only read that file or directory, otherwise elm-build has no way to know when to re-run it.

-}
commandWithFile :
    String
    -> List String
    -> FileOrDirectory
    -> (FileOrDirectory -> Monad a)
    -> Monad a
commandWithFile cmd args hash k =
    let
        outputHash : FileOrDirectory
        outputHash =
            extendHashWith (cmd :: args) hash
    in
    (derive {- (String.join " " ("commandWithFile" :: cmd :: args)) -} outputHash <| \{ prefix, buildPath } target ->
    Do.do (commandLog prefix cmd (args ++ [ hashToPath buildPath hash ])) <| \output ->
    BackendTask.allowFatal (Script.writeFile { path = hashToPath buildPath target, body = output })
    )
        |> andThen k


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
withFile : FileOrDirectory -> (String -> Monad a) -> (a -> Monad b) -> Monad b
withFile hash f k =
    Monad "withFile"
        (\({ buildPath } as input_) deps ->
            Do.allowFatal (File.rawFile (hashToPath buildPath hash)) <| \raw ->
            runMonad (f raw) input_ (hashSetInsert hash deps)
        )
        |> andThen k


{-| -}
writeFile : String -> (FileOrDirectory -> Monad a) -> Monad a
writeFile content k =
    let
        hash : FileOrDirectory
        hash =
            stringToHash content
    in
    (derive {- "writeFile" -} hash <| \{ buildPath } target ->
    BackendTask.allowFatal (Script.writeFile { path = hashToPath buildPath target, body = content })
    )
        |> andThen k


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
    runMonad m input_ hashSetEmpty
        |> BackendTask.map
            (\( output, deps ) ->
                { output = Path.path (hashToPath buildPath output)
                , dependencies =
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
        |> BackendTask.map
            (\raw ->
                HashSet (innerHashSetFromList raw)
            )


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
                                            ( res :: resAcc, hashSetUnion newDeps depsAcc )
                                        )
                                        ( [], hashSetEmpty )
                            )
        )


{-| -}
each : List a -> (a -> Monad b) -> (List b -> Monad c) -> Monad c
each l f k =
    l |> List.map f |> sequence |> andThen k


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
                                |> Tuple.mapSecond (List.foldl hashSetUnion hashSetEmpty)
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


jobs : (Int -> Monad a) -> Monad a
jobs k =
    andThen k jobs_


jobs_ : Monad Int
jobs_ =
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



------------
-- HASHES --
------------


{-| -}
type FileOrDirectory
    = Hash String


{-| Build an hashed file from the output of shaXsum/b3sum.
-}
inputHash : String -> Result String FileOrDirectory
inputHash raw =
    let
        clean : String
        clean =
            String.left 8 raw
    in
    Hex.fromString clean
        |> Result.map (\_ -> Hash clean)


hashToPath : Path -> FileOrDirectory -> String
hashToPath buildPath (Hash hash) =
    Path.toString buildPath ++ "/" ++ hash


hashToString : FileOrDirectory -> String
hashToString (Hash hash) =
    hash


hashToTmp : FileOrDirectory -> FileOrDirectory
hashToTmp (Hash hash) =
    Hash ("tmp-" ++ hash)


hashToWorkspace : FileOrDirectory -> FileOrDirectory
hashToWorkspace (Hash hash) =
    Hash ("workspace-" ++ hash)


extendHashWith : List String -> FileOrDirectory -> FileOrDirectory
extendHashWith l (Hash r) =
    stringToHash (String.join "|" (r :: l))


stringToHash : String -> FileOrDirectory
stringToHash raw =
    raw
        |> FNV1a.hash
        |> Hex.toString
        |> String.padLeft 8 '0'
        |> Hash


type HashSet
    = HashSet (BST String)


type BST a
    = BSTNode a (BST a) (BST a)
    | BSTLeaf


hashSetEmpty : HashSet
hashSetEmpty =
    HashSet BSTLeaf


hashSetMember : FileOrDirectory -> HashSet -> Bool
hashSetMember (Hash x) (HashSet s) =
    innerHashSetMember x s


innerHashSetMember : comparable -> BST comparable -> Bool
innerHashSetMember k t =
    case t of
        BSTLeaf ->
            False

        BSTNode nk l r ->
            case compare k nk of
                EQ ->
                    True

                LT ->
                    innerHashSetMember k l

                GT ->
                    innerHashSetMember k r


hashSetInsert : FileOrDirectory -> HashSet -> HashSet
hashSetInsert (Hash x) (HashSet s) =
    HashSet (innerHashSetInsert x s |> Maybe.withDefault s)


innerHashSetInsert : comparable -> BST comparable -> Maybe (BST comparable)
innerHashSetInsert k t =
    case t of
        BSTLeaf ->
            Just (BSTNode k BSTLeaf BSTLeaf)

        BSTNode nk l r ->
            case compare k nk of
                EQ ->
                    Nothing

                LT ->
                    case innerHashSetInsert k l of
                        Nothing ->
                            Nothing

                        Just nl ->
                            Just (BSTNode nk nl r)

                GT ->
                    case innerHashSetInsert k r of
                        Nothing ->
                            Nothing

                        Just nr ->
                            Just (BSTNode nk l nr)


hashSetUnion : HashSet -> HashSet -> HashSet
hashSetUnion (HashSet a) (HashSet b) =
    HashSet (innerHashSetUnion a b)


innerHashSetUnion : BST comparable -> BST comparable -> BST comparable
innerHashSetUnion l r =
    case l of
        BSTLeaf ->
            r

        BSTNode lk ll lr ->
            let
                ( rl, rr ) =
                    split lk r
            in
            BSTNode lk (innerHashSetUnion ll rl) (innerHashSetUnion lr rr)


split : comparable -> BST comparable -> ( BST comparable, BST comparable )
split k t =
    case t of
        BSTLeaf ->
            ( BSTLeaf, BSTLeaf )

        BSTNode nk l r ->
            case compare k nk of
                EQ ->
                    ( l, r )

                LT ->
                    let
                        ( ll, lr ) =
                            split k l
                    in
                    ( ll, BSTNode nk lr r )

                GT ->
                    let
                        ( rl, rr ) =
                            split k r
                    in
                    ( BSTNode nk l rl, rr )


hashSetToList : HashSet -> List FileOrDirectory
hashSetToList (HashSet s) =
    innerHashSetToList s
        |> List.map Hash


innerHashSetToList : BST a -> List a
innerHashSetToList s =
    let
        go : BST a -> List a -> List a
        go t acc =
            case t of
                BSTLeaf ->
                    acc

                BSTNode k l r ->
                    go l acc
                        |> (::) k
                        |> go r
    in
    go s []


innerHashSetFromList : List comparable -> BST comparable
innerHashSetFromList list =
    let
        arr : Array comparable
        arr =
            Array.fromList (List.sort list)

        go fromIncluded toExcluded =
            if fromIncluded >= toExcluded then
                BSTLeaf

            else
                let
                    mid : Int
                    mid =
                        fromIncluded + (toExcluded - fromIncluded) // 2
                in
                case Array.get mid arr of
                    Nothing ->
                        BSTLeaf

                    Just k ->
                        BSTNode k (go fromIncluded mid) (go (mid + 1) toExcluded)
    in
    go 0 (Array.length arr)
