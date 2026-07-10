module BuildTask exposing
    ( BuildTask, Command, Error(..)
    , FileOrDirectory, input, inputs, downloadSHA256, DownloadError(..)
    , do, doWithError, succeed, fail
    , writeFile, writeBytes, run
    , map, map2, map3, map4, map5, andMap, andThen, andThen2, combine, combineBy, combineInto, each, sequence, toResult, mapError, mapRecoverableError, allowFatal
    , withFile, readFromDirectory
    , withPrefix, timed
    , Warning, withWarning, withWarnings
    , jobs, triggerDebugger, fromResult
    , withEnv, withMemoryLimitInGB, withDebug, withIdlePriority
    , getTool, which
    )

{-|


## Types

@docs BuildTask, Command, Error


## Input

@docs FileOrDirectory, input, inputs, downloadSHA256, DownloadError


## Building blocks

@docs Monad, do, doWithError, succeed, fail


## Output

@docs writeFile, writeBytes, run


## Transforming and combining `Monad` values

@docs map, map2, map3, map4, map5, andMap, andThen, andThen2, combine, combineBy, combineInto, each, sequence, toResult, mapError, mapRecoverableError, allowFatal


## Operations

@docs commandWithFile, commandInReadonlyDirectory, commandInWritableDirectory, withFile, readFromDirectory


## Output control

@docs withPrefix, timed


## Warnings

@docs Warning, withWarning, withWarnings


## Utils

@docs jobs, triggerDebugger, fromResult


## Advanced

@docs withEnv, withMemoryLimitInGB, withDebug, withIdlePriority

-}

import BackendTask exposing (BackendTask)
import BackendTask.Customs
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BuildTask.Internal as Internal
import Bytes exposing (Bytes)
import FastDict as Dict exposing (Dict)
import FastSet exposing (Set)
import FatalError exposing (FatalError)
import Hash exposing (Hash)
import Pages.Script as Script
import Path exposing (Path)
import Result.Extra
import SHA256


{-| `tools` is the commands that the task can run
-}
type alias BuildTask tools e a =
    Internal.BuildTask tools e a


{-| -}
type Error e
    = InternalError FatalError
    | UserError e


type alias Command =
    { name : String
    , hash : FileOrDirectory
    }


type alias FileOrDirectory =
    Hash Hash.Normal


type alias Warning =
    Internal.Warning


{-| -}
succeed : a -> BuildTask tools e a
succeed v =
    Internal.succeed v


{-| -}
map : (a -> b) -> BuildTask tools e a -> BuildTask tools e b
map f m =
    Internal.map f m


{-| -}
map2 :
    (a -> b -> c)
    -> BuildTask tools e a
    -> BuildTask tools e b
    -> BuildTask tools e c
map2 f a b =
    Internal.map2 f a b


{-| -}
map3 :
    (a -> b -> c -> d)
    -> BuildTask tools e a
    -> BuildTask tools e b
    -> BuildTask tools e c
    -> BuildTask tools e d
map3 f a b c =
    Internal.map3 f a b c


{-| -}
map4 :
    (a -> b -> c -> d -> f)
    -> BuildTask tools e a
    -> BuildTask tools e b
    -> BuildTask tools e c
    -> BuildTask tools e d
    -> BuildTask tools e f
map4 f a b c d =
    Internal.map4 f a b c d


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
    Internal.map5 f a b c d e


andMap : BuildTask tools e a -> BuildTask tools e (a -> b) -> BuildTask tools e b
andMap f x =
    map2 (|>) f x


{-| -}
mapError : (e -> f) -> BuildTask tools e a -> BuildTask tools f a
mapError f t =
    Internal.mapError f t


{-| -}
andThen : (a -> BuildTask tools e b) -> BuildTask tools e a -> BuildTask tools e b
andThen f m =
    Internal.andThen f m


andThen2 :
    (a -> b -> BuildTask tools e c)
    -> BuildTask tools e a
    -> BuildTask tools e b
    -> BuildTask tools e c
andThen2 f l r =
    Internal.andThen2 f l r


{-| -}
fail : e -> BuildTask tools e a
fail msg =
    Internal.fail msg


{-| -}
triggerDebugger : BuildTask tools e ()
triggerDebugger =
    Internal.triggerDebugger


{-| -}
input : Path -> BuildTask tools FatalError FileOrDirectory
input inputPath =
    Internal.input inputPath


{-| -}
type DownloadError
    = WrongHashLength String
    | InvalidHashHex String
    | DownloadError FatalError
    | WrongHash { expected : String, actual : String }


{-| -}
downloadSHA256 : { url : String, sha256 : String } -> BuildTask { tools | sha256sum : Command } DownloadError FileOrDirectory
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
    { input | buildPath : Path, debug : Bool }
    -> List Path
    ->
        BackendTask
            FatalError
            (List
                ( Path
                , BuildTask tools e FileOrDirectory
                )
            )
inputs input_ inputPaths =
    -- TODO: copy the files inside the build path _before_ hashing
    Do.do Internal.getInternalTools <| \internalTools ->
    Do.do
        (Internal.commandLog
            { memoryLimit = Nothing
            , prefix = []
            , env = Dict.empty
            , idlePriority = False
            , debug = input_.debug
            }
            internalTools.b3sum.name
            (List.map Path.toString inputPaths)
            |> BackendTask.allowFatal
        )
    <| \body ->
    List.map2
        (\inputPath line ->
            case Hash.fromChecksum line of
                Ok hash ->
                    ( inputPath
                    , Internal.derive "inputs" hash <| \{ buildPath } target ->
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
combineInto : List { filename : Path, hash : FileOrDirectory } -> BuildTask tools e FileOrDirectory
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


combineTree : Tree -> BuildTask tools e FileOrDirectory
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

        outputHash : BuildTask tools e FileOrDirectory
        outputHash =
            Dict.foldl
                (\filename hash acc -> filename :: Hash.toString hash :: acc)
                [ "combineTree" ]
                combined
                |> String.join "|"
                |> Internal.hashFromString
    in
    do outputHash <| \combinedHash ->
    Internal.derive "combine" combinedHash <| \({ internalTools, buildPath } as input_) target ->
    Do.do
        (Script.makeDirectory { recursive = True } (Hash.toPathTemporary buildPath target)
            |> BackendTask.mapError Internal.InternalError
        )
    <| \_ ->
    combined
        |> Dict.foldl (\outputFilename hash acc -> Internal.execLog input_ internalTools.cp.name [ "-rl", Hash.toPath buildPath hash, Hash.toPathTemporary buildPath target ++ "/" ++ outputFilename ] :: acc) []
        |> BackendTask.Extra.combineBy_ parallelism
        |> BackendTask.mapError Internal.InternalError


{-| -}
do : BuildTask tools e b -> (b -> BuildTask tools e a) -> BuildTask tools e a
do x f =
    andThen f x


doWithError :
    BuildTask tools { recoverable : e, fatal : FatalError } a
    -> (e -> f)
    -> (a -> BuildTask tools { recoverable : f, fatal : FatalError } b)
    -> BuildTask tools { recoverable : f, fatal : FatalError } b
doWithError x e f =
    mapRecoverableError e x |> andThen f


{-| -}
withFile :
    FileOrDirectory
    -> (String -> BuildTask tools { fatal : FatalError, recoverable : File.FileReadError decoderError } a)
    -> BuildTask tools { fatal : FatalError, recoverable : File.FileReadError decoderError } a
withFile hash f =
    Internal.withFile hash f


{-| -}
writeFile : String -> BuildTask tools { fatal : FatalError, recoverable : Script.Error } FileOrDirectory
writeFile content =
    do (Internal.hashFromString content) <| \hash ->
    Internal.derive "writeFile" hash <| \{ buildPath } target ->
    Script.writeFile { path = Hash.toPathTemporary buildPath target, body = content }
        |> BackendTask.mapError Internal.UserError


writeBytes : Bytes -> BuildTask tools FatalError FileOrDirectory
writeBytes content =
    case Hash.fromChecksum (SHA256.toHex (SHA256.fromBytes content)) of
        Err e ->
            Internal.fail (FatalError.fromString e) |> Internal.fatalToInternal

        Ok hash ->
            Internal.derive "writeBytes" hash <| \{ buildPath } target ->
            BackendTask.Customs.writeBinaryFile { path = Hash.toPathTemporary buildPath target, body = content }
                |> BackendTask.mapError Internal.UserError


{-| -}
run :
    { jobs : Maybe Int
    , debug : Bool
    , check : Bool
    , hashKind : Hash.Kind
    , keepFailed : Bool
    , getTools : BuildTask () e tools
    }
    -> Path
    -> BuildTask tools e FileOrDirectory
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
combineBy : Int -> List (BuildTask tools e a) -> BuildTask tools e (List a)
combineBy n ops =
    Internal.combineBy n ops


{-| -}
combine : List (BuildTask tools e a) -> BuildTask tools e (List a)
combine inputs_ =
    do jobs <| \parallelism ->
    combineBy parallelism inputs_


{-| -}
each : List a -> (a -> BuildTask tools e b) -> BuildTask tools e (List b)
each l f =
    l |> List.map f |> sequence


{-| -}
sequence : List (BuildTask tools e a) -> BuildTask tools e (List a)
sequence ops =
    Internal.sequence ops


{-| -}
timed : String -> String -> BuildTask tools e a -> BuildTask tools e a
timed before after task =
    Internal.timed before after task


{-| -}
withPrefix : String -> BuildTask tools e a -> BuildTask tools e a
withPrefix newPrefix m =
    Internal.withPrefix newPrefix m


jobs : BuildTask tools e Int
jobs =
    Internal.jobs


toResult : BuildTask tools e a -> BuildTask tools x (Result e a)
toResult task =
    Internal.toResult task


withWarnings : List Warning -> BuildTask tools e a -> BuildTask tools e a
withWarnings warnings task =
    List.foldl withWarning task warnings


withWarning : Warning -> BuildTask tools e a -> BuildTask tools e a
withWarning warning task =
    Internal.withWarning warning task


fromResult : Result e a -> BuildTask tools e a
fromResult res =
    case res of
        Ok o ->
            succeed o

        Err e ->
            fail e


withEnv : List ( String, String ) -> BuildTask tools e a -> BuildTask tools e a
withEnv newEnv task =
    Internal.withEnv newEnv task


{-| -}
mapRecoverableError :
    (e -> f)
    -> BuildTask tools { fatal : FatalError, recoverable : e } a
    -> BuildTask tools { fatal : FatalError, recoverable : f } a
mapRecoverableError f task =
    mapError (\{ fatal, recoverable } -> { fatal = fatal, recoverable = f recoverable }) task


{-| -}
allowFatal : BuildTask tools { e | fatal : FatalError } a -> BuildTask tools FatalError a
allowFatal task =
    Internal.allowFatal task


readFromDirectory : FileOrDirectory -> String -> BuildTask tools { fatal : FatalError, recoverable : File.FileReadError e } String
readFromDirectory directory file =
    do (Internal.extractFromDirectory directory file) <| \extracted ->
    withFile extracted succeed


withMemoryLimitInGB : Int -> BuildTask tools e a -> BuildTask tools e a
withMemoryLimitInGB limit task =
    Internal.withMemoryLimitInBytes (limit * 1024 * 1024 * 1024) task


withDebug : (String -> Never) -> BuildTask tools e a -> BuildTask tools e a
withDebug todo task =
    Internal.withDebug todo task


withIdlePriority : BuildTask tools e a -> BuildTask tools e a
withIdlePriority task =
    Internal.withIdlePriority task


which : String -> BuildTask tools FatalError Command
which name =
    Internal.which name


getTool : (tools -> command) -> BuildTask tools e command
getTool f =
    Internal.getTool f
