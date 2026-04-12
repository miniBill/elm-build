module BuildTask exposing
    ( FileOrDirectory, input, inputs, downloadSHA256
    , do, succeed, fail
    , writeFile, run
    , map, map2, map3, map4, andThen, combine, combineBy, each, sequence, toResult
    , withFile
    , withPrefix, timed
    , jobs, triggerDebugger
    , BuildTask
    )

{-|


## Input

@docs FileOrDirectory, input, inputs, downloadSHA256


## Building blocks

@docs Monad, do, succeed, fail


## Output

@docs writeFile, run


## Transforming and combining `Monad` values

@docs map, map2, map3, map4, andThen, combine, combineBy, each, sequence, toResult


## Operations

@docs commandWithFile, commandInReadonlyDirectory, commandInWritableDirectory, withFile


## Output control

@docs withPrefix, timed


## Utils

@docs jobs, triggerDebugger

-}

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Extra
import BuildTask.Internal as Internal
import FastDict as Dict exposing (Dict)
import FatalError exposing (FatalError)
import Hash exposing (Hash)
import Pages.Script as Script
import Path exposing (Path)
import Result.Extra


{-| -}
type alias BuildTask a =
    Internal.BuildTask a


type alias FileOrDirectory =
    Hash Hash.Normal


{-| -}
succeed : a -> BuildTask a
succeed v =
    Internal.succeed v


{-| -}
map : (a -> b) -> BuildTask a -> BuildTask b
map f m =
    Internal.map f m


{-| -}
map2 :
    (a -> b -> c)
    -> BuildTask a
    -> BuildTask b
    -> BuildTask c
map2 f a b =
    Internal.map2 f a b


{-| -}
map3 :
    (a -> b -> c -> d)
    -> BuildTask a
    -> BuildTask b
    -> BuildTask c
    -> BuildTask d
map3 f a b c =
    Internal.map3 f a b c


{-| -}
map4 :
    (a -> b -> c -> d -> e)
    -> BuildTask a
    -> BuildTask b
    -> BuildTask c
    -> BuildTask d
    -> BuildTask e
map4 f a b c d =
    Internal.map4 f a b c d


{-| -}
andThen : (a -> BuildTask b) -> BuildTask a -> BuildTask b
andThen f m =
    Internal.andThen f m


{-| -}
fail : String -> BuildTask a
fail msg =
    Internal.fail msg


{-| -}
triggerDebugger : BuildTask ()
triggerDebugger =
    Internal.triggerDebugger


{-| -}
input : Path -> BuildTask FileOrDirectory
input inputPath =
    Internal.input inputPath


{-| -}
downloadSHA256 : { url : String, sha256 : String } -> BuildTask FileOrDirectory
downloadSHA256 config =
    Internal.downloadSHA256 config


{-| -}
inputs :
    List Path
    ->
        BackendTask
            FatalError
            (List
                ( Path
                , BuildTask FileOrDirectory
                )
            )
inputs inputPaths =
    -- TODO: copy the files inside the build path _before_ hashing
    Do.do (Internal.commandLog [] "b3sum" (List.map Path.toString inputPaths)) <| \body ->
    List.map2
        (\inputPath line ->
            case Hash.fromChecksum line of
                Ok hash ->
                    ( inputPath
                    , Internal.derive "inputs" hash <| \{ prefix, buildPath } target ->
                    Internal.execLog prefix "cp" [ Path.toString inputPath, Hash.toPathTemporary buildPath target ]
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
combine : List { filename : Path, hash : FileOrDirectory } -> BuildTask FileOrDirectory
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


combineTree : Tree -> BuildTask FileOrDirectory
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

        outputHash : BuildTask FileOrDirectory
        outputHash =
            Dict.foldl
                (\filename hash acc -> filename :: Hash.toString hash :: acc)
                [ "combineTree" ]
                combined
                |> String.join "|"
                |> Internal.hashFromString
    in
    do outputHash <| \combinedHash ->
    Internal.derive "combine" combinedHash <| \{ prefix, buildPath } target ->
    Do.do (Internal.execLog prefix "mkdir" [ "-p", Hash.toPathTemporary buildPath target ]) <| \_ ->
    combined
        |> Dict.foldl (\outputFilename hash acc -> Internal.execLog prefix "cp" [ "-rl", Hash.toPath buildPath hash, Hash.toPathTemporary buildPath target ++ "/" ++ outputFilename ] :: acc) []
        |> BackendTask.Extra.combineBy_ parallelism


{-| -}
do : BuildTask b -> (b -> BuildTask a) -> BuildTask a
do x f =
    andThen f x


{-| -}
withFile : FileOrDirectory -> (String -> BuildTask a) -> BuildTask a
withFile hash f =
    Internal.withFile hash f


{-| -}
writeFile : String -> BuildTask FileOrDirectory
writeFile content =
    do (Internal.hashFromString content) <| \hash ->
    Internal.derive "writeFile" hash <| \{ buildPath } target ->
    BackendTask.allowFatal (Script.writeFile { path = Hash.toPathTemporary buildPath target, body = content })


{-| -}
run : { jobs : Maybe Int, debug : Bool, hashKind : Hash.Kind } -> Path -> BuildTask FileOrDirectory -> BackendTask FatalError { output : Path, intermediate : List Path }
run config buildPath m =
    Internal.run config buildPath m


{-| -}
combineBy : Int -> List (BuildTask a) -> BuildTask (List a)
combineBy n ops =
    Internal.combineBy n ops


{-| -}
each : List a -> (a -> BuildTask b) -> BuildTask (List b)
each l f =
    l |> List.map f |> sequence


{-| -}
sequence : List (BuildTask a) -> BuildTask (List a)
sequence ops =
    Internal.sequence ops


{-| -}
timed : String -> String -> BuildTask a -> BuildTask a
timed before after task =
    Internal.timed before after task


{-| -}
withPrefix : String -> BuildTask a -> BuildTask a
withPrefix newPrefix m =
    Internal.withPrefix newPrefix m


jobs : BuildTask Int
jobs =
    Internal.jobs


toResult : BuildTask a -> BuildTask (Result FatalError a)
toResult task =
    Internal.toResult task
