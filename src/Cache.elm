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
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.Stream as Stream
import Cache.Internal as Internal exposing (Monad)
import FastDict as Dict exposing (Dict)
import FatalError exposing (FatalError)
import Pages.Script as Script
import Path exposing (Path)
import Result.Extra


{-| -}
type alias Monad a =
    Internal.Monad a


type alias FileOrDirectory =
    Internal.Hash


{-| -}
succeed : a -> Monad a
succeed v =
    Internal.succeed v


{-| -}
map : (a -> b) -> Monad a -> Monad b
map f m =
    Internal.map f m


{-| -}
map2 :
    (a -> b -> c)
    -> Monad a
    -> Monad b
    -> Monad c
map2 f a b =
    Internal.map2 f a b


{-| -}
map3 :
    (a -> b -> c -> d)
    -> Monad a
    -> Monad b
    -> Monad c
    -> Monad d
map3 f a b c =
    Internal.map3 f a b c


{-| -}
map4 :
    (a -> b -> c -> d -> e)
    -> Monad a
    -> Monad b
    -> Monad c
    -> Monad d
    -> Monad e
map4 f a b c d =
    Internal.map4 f a b c d


{-| -}
andThen : (a -> Monad b) -> Monad a -> Monad b
andThen f m =
    Internal.andThen f m


{-| -}
fail : String -> Monad a
fail msg =
    Internal.fail msg


{-| -}
triggerDebugger : Monad ()
triggerDebugger =
    Internal.triggerDebugger


{-| -}
input : Path -> Monad FileOrDirectory
input inputPath =
    Internal.input inputPath


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
    Do.do (Internal.commandLog [] "b3sum" (List.map Path.toString inputPaths)) <| \body ->
    List.map2
        (\inputPath line ->
            case Internal.inputHash line of
                Ok hash ->
                    ( inputPath
                    , Internal.derive {- "inputs" -} hash <| \{ prefix, buildPath } target ->
                    Internal.execLog prefix "cp" [ Path.toString inputPath, Internal.hashToPath buildPath target ]
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
    Do.do (Internal.execLog prefix "mkdir" [ "-p", Internal.hashToPath buildPath target ]) <| \_ ->
    combined
        |> Dict.foldl (\outputFilename hash acc -> Internal.execLog prefix "cp" [ "-rl", Internal.hashToPath buildPath hash, Internal.hashToPath buildPath target ++ "/" ++ outputFilename ] :: acc) []
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
    Do.do (Internal.commandLog prefix cmd args |> BackendTask.inDir (Internal.hashToPath buildPath hash)) <| \output ->
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
        (Internal.commandLog prefix cmd args
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
    Do.do (Internal.commandLog prefix cmd (args ++ [ Internal.hashToPath buildPath hash ])) <| \output ->
    BackendTask.allowFatal (Script.writeFile { path = Internal.hashToPath buildPath target, body = output })


{-| -}
do : Monad b -> (b -> Monad a) -> Monad a
do x f =
    andThen f x


{-| -}
withFile : FileOrDirectory -> (String -> Monad a) -> Monad a
withFile hash f =
    Internal.withFile hash f


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
run : { jobs : Maybe Int } -> Path -> Monad FileOrDirectory -> BackendTask FatalError { output : Path, intermediate : List Path }
run config buildPath m =
    Internal.run config buildPath m


{-| -}
combineBy : Int -> List (Monad a) -> Monad (List a)
combineBy n ops =
    Internal.combineBy n ops


{-| -}
each : List a -> (a -> Monad b) -> Monad (List b)
each l f =
    l |> List.map f |> sequence


{-| -}
sequence : List (Monad a) -> Monad (List a)
sequence ops =
    Internal.sequence ops


{-| -}
timed : String -> String -> Monad a -> Monad a
timed before after task =
    Internal.timed before after task


{-| -}
withPrefix : String -> Monad a -> Monad a
withPrefix newPrefix m =
    Internal.withPrefix newPrefix m


jobs : Monad Int
jobs =
    Internal.jobs
