module BuildTask exposing
    ( BuildTask, Error(..)
    , FileOrDirectory, input, inputs, downloadSHA256, DownloadError(..)
    , do, doWithError, succeed, fail
    , writeFile, run
    , map, map2, map3, map4, map5, andThen, andThen2, combine, combineBy, combineInto, each, sequence, toResult, mapError, mapRecoverableError, allowFatal
    , withFile
    , withPrefix, timed
    , Warning, withWarning, withWarnings
    , jobs, triggerDebugger, fromResult
    , withEnv
    )

{-|


## Types

@docs BuildTask, Error


## Input

@docs FileOrDirectory, input, inputs, downloadSHA256, DownloadError


## Building blocks

@docs Monad, do, doWithError, succeed, fail


## Output

@docs writeFile, run


## Transforming and combining `Monad` values

@docs map, map2, map3, map4, map5, andThen, andThen2, combine, combineBy, combineInto, each, sequence, toResult, mapError, mapRecoverableError, allowFatal


## Operations

@docs commandWithFile, commandInReadonlyDirectory, commandInWritableDirectory, withFile


## Output control

@docs withPrefix, timed


## Warnings

@docs Warning, withWarning, withWarnings


## Utils

@docs jobs, triggerDebugger, fromResult


## Advanced

@docs withEnv

-}

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BuildTask.Internal as Internal
import FastDict as Dict exposing (Dict)
import FastSet exposing (Set)
import FatalError exposing (FatalError, recoverable)
import Hash exposing (Hash)
import Pages.Script as Script
import Path exposing (Path)
import Result.Extra


{-| -}
type alias BuildTask e a =
    Internal.BuildTask e a


{-| -}
type Error e
    = InternalError FatalError
    | UserError e


type alias FileOrDirectory =
    Hash Hash.Normal


type alias Warning =
    Internal.Warning


{-| -}
succeed : a -> BuildTask e a
succeed v =
    Internal.succeed v


{-| -}
map : (a -> b) -> BuildTask e a -> BuildTask e b
map f m =
    Internal.map f m


{-| -}
map2 :
    (a -> b -> c)
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
map2 f a b =
    Internal.map2 f a b


{-| -}
map3 :
    (a -> b -> c -> d)
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
    -> BuildTask e d
map3 f a b c =
    Internal.map3 f a b c


{-| -}
map4 :
    (a -> b -> c -> d -> f)
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
    -> BuildTask e d
    -> BuildTask e f
map4 f a b c d =
    Internal.map4 f a b c d


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
    Internal.map5 f a b c d e


{-| -}
mapError : (e -> f) -> BuildTask e a -> BuildTask f a
mapError f t =
    Internal.mapError f t


{-| -}
andThen : (a -> BuildTask e b) -> BuildTask e a -> BuildTask e b
andThen f m =
    Internal.andThen f m


andThen2 :
    (a -> b -> BuildTask e c)
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
andThen2 f l r =
    Internal.andThen2 f l r


{-| -}
fail : e -> BuildTask e a
fail msg =
    Internal.fail msg


{-| -}
triggerDebugger : BuildTask e ()
triggerDebugger =
    Internal.triggerDebugger


{-| -}
input : Path -> BuildTask FatalError FileOrDirectory
input inputPath =
    Internal.input inputPath


{-| -}
type DownloadError
    = WrongHashLength String
    | InvalidHashHex String
    | DownloadError FatalError
    | WrongHash { expected : String, actual : String }


{-| -}
downloadSHA256 : { url : String, sha256 : String } -> BuildTask DownloadError FileOrDirectory
downloadSHA256 config =
    Internal.downloadSHA256 config
        |> mapError
            (\e ->
                case e of
                    Internal.WrongHashLength hash ->
                        WrongHashLength hash

                    Internal.InvalidHashHex d ->
                        InvalidHashHex d

                    Internal.DownloadError d ->
                        DownloadError d

                    Internal.WrongHash d ->
                        WrongHash d
            )


{-| -}
inputs :
    List Path
    ->
        BackendTask
            FatalError
            (List
                ( Path
                , BuildTask e FileOrDirectory
                )
            )
inputs inputPaths =
    -- TODO: copy the files inside the build path _before_ hashing
    Do.do
        (Internal.commandLog [] "b3sum" (List.map Path.toString inputPaths)
            |> BackendTask.allowFatal
        )
    <| \body ->
    List.map2
        (\inputPath line ->
            case Hash.fromChecksum line of
                Ok hash ->
                    ( inputPath
                    , Internal.derive "inputs" hash <| \{ prefix, buildPath } target ->
                    Script.copyFile
                        { from = Path.toString inputPath
                        , to = Hash.toPathTemporary buildPath target
                        }
                        |> BackendTask.mapError Internal.InternalError
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
combineInto : List { filename : Path, hash : FileOrDirectory } -> BuildTask e FileOrDirectory
combineInto files =
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


combineTree : Tree -> BuildTask e FileOrDirectory
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

        outputHash : BuildTask e FileOrDirectory
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
    Do.do
        (Script.makeDirectory { recursive = True } (Hash.toPathTemporary buildPath target)
            |> BackendTask.mapError Internal.InternalError
        )
    <| \_ ->
    combined
        |> Dict.foldl (\outputFilename hash acc -> Internal.execLog prefix "cp" [ "-rl", Hash.toPath buildPath hash, Hash.toPathTemporary buildPath target ++ "/" ++ outputFilename ] :: acc) []
        |> BackendTask.Extra.combineBy_ parallelism
        |> BackendTask.mapError Internal.InternalError


{-| -}
do : BuildTask e b -> (b -> BuildTask e a) -> BuildTask e a
do x f =
    andThen f x


doWithError :
    BuildTask { recoverable : e, fatal : FatalError } a
    -> (e -> f)
    -> (a -> BuildTask { recoverable : f, fatal : FatalError } b)
    -> BuildTask { recoverable : f, fatal : FatalError } b
doWithError x e f =
    andThen f (mapRecoverableError e x)


{-| -}
withFile :
    FileOrDirectory
    -> (String -> BuildTask { fatal : FatalError, recoverable : File.FileReadError decoderError } a)
    -> BuildTask { fatal : FatalError, recoverable : File.FileReadError decoderError } a
withFile hash f =
    Internal.withFile hash f


{-| -}
writeFile : String -> BuildTask { fatal : FatalError, recoverable : Script.Error } FileOrDirectory
writeFile content =
    do (Internal.hashFromString content) <| \hash ->
    Internal.derive "writeFile" hash <| \{ buildPath } target ->
    Script.writeFile { path = Hash.toPathTemporary buildPath target, body = content }
        |> BackendTask.mapError Internal.UserError


{-| -}
run :
    { jobs : Maybe Int, debug : Bool, check : Bool, hashKind : Hash.Kind }
    -> Path
    -> BuildTask e FileOrDirectory
    -> BackendTask (Error e) { output : Path, intermediate : List Path, warnings : Set Warning }
run config buildPath m =
    Internal.run config buildPath m
        |> BackendTask.mapError
            (\e ->
                case e of
                    Internal.InternalError i ->
                        InternalError i

                    Internal.UserError u ->
                        UserError u
            )


{-| -}
combineBy : Int -> List (BuildTask e a) -> BuildTask e (List a)
combineBy n ops =
    Internal.combineBy n ops


{-| -}
combine : List (BuildTask e a) -> BuildTask e (List a)
combine inputs_ =
    do jobs <| \parallelism ->
    combineBy parallelism inputs_


{-| -}
each : List a -> (a -> BuildTask e b) -> BuildTask e (List b)
each l f =
    l |> List.map f |> sequence


{-| -}
sequence : List (BuildTask e a) -> BuildTask e (List a)
sequence ops =
    Internal.sequence ops


{-| -}
timed : String -> String -> BuildTask e a -> BuildTask e a
timed before after task =
    Internal.timed before after task


{-| -}
withPrefix : String -> BuildTask e a -> BuildTask e a
withPrefix newPrefix m =
    Internal.withPrefix newPrefix m


jobs : BuildTask e Int
jobs =
    Internal.jobs


toResult : BuildTask e a -> BuildTask x (Result e a)
toResult task =
    Internal.toResult task


withWarnings : List Warning -> BuildTask e a -> BuildTask e a
withWarnings warnings task =
    List.foldl withWarning task warnings


withWarning : Warning -> BuildTask e a -> BuildTask e a
withWarning warning task =
    Internal.withWarning warning task


fromResult : Result e a -> BuildTask e a
fromResult res =
    case res of
        Ok o ->
            succeed o

        Err e ->
            fail e


withEnv : List ( String, String ) -> BuildTask e a -> BuildTask e a
withEnv newEnv task =
    Internal.withEnv newEnv task


{-| -}
mapRecoverableError :
    (e -> f)
    -> BuildTask { fatal : FatalError, recoverable : e } a
    -> BuildTask { fatal : FatalError, recoverable : f } a
mapRecoverableError f task =
    mapError (\{ fatal, recoverable } -> { fatal = fatal, recoverable = f recoverable }) task


{-| -}
allowFatal : BuildTask { e | fatal : FatalError } a -> BuildTask FatalError a
allowFatal task =
    Internal.allowFatal task
