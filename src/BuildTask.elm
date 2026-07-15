module BuildTask exposing
    ( BuildTask, Command, Error(..)
    , FileOrDirectory, input, inputs, downloadSHA256, DownloadError(..)
    , do, doWithError, succeed, fail
    , writeFile, writeBytes, run
    , map, map2, map3, map4, map5, andMap, andThen, andThen2, combine, combineBy, combineInto, each, sequence, toResult, mapError, mapRecoverableError, allowFatal
    , withFile, readFromDirectory
    , withPrefix, timed
    , Warning, withWarning, withWarnings
    , jobs, triggerDebugger, fromResult, which
    , withEnv, withMemoryLimitInMB, withMemoryLimitInGB, withDebug, withIdlePriority
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

@docs jobs, triggerDebugger, fromResult, which


## Advanced

@docs withEnv, withMemoryLimitInMB, withMemoryLimitInGB, withDebug, withIdlePriority

-}

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.File.Extra
import BuildTask.Internal as Internal
import Bytes exposing (Bytes)
import FastDict as Dict
import FastSet exposing (Set)
import FatalError exposing (FatalError)
import Hash exposing (Hash)
import Pages.Script as Script
import Path.Posix as Path exposing (Path)
import PathDict exposing (PathDict)
import Result.Extra
import SHA256


{-| `tools` is the commands that the task can run
-}
type alias BuildTask e a =
    Internal.BuildTask e a


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


andMap : BuildTask e a -> BuildTask e (a -> b) -> BuildTask e b
andMap f x =
    map2 (|>) f x


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
input : Path base Path.File -> BuildTask FatalError FileOrDirectory
input inputPath =
    Internal.inputFile inputPath


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
    { input | buildPath : String, debug : Bool }
    -> List (Path base Path.File)
    ->
        BackendTask
            FatalError
            (List
                ( Path base Path.File
                , BuildTask e FileOrDirectory
                )
            )
inputs input_ inputPaths =
    -- TODO: copy the files inside the build path _before_ hashing
    Do.do
        (Internal.commandLog
            { memoryLimit = Nothing
            , prefix = []
            , env = Dict.empty
            , idlePriority = False
            , debug = input_.debug
            , buildPath = Path.root
            }
            "b3sum"
            (List.map Path.toString inputPaths)
            |> BackendTask.allowFatal
        )
    <| \body ->
    List.map2
        (\inputPath line ->
            case Hash.fromChecksum line of
                Ok hash ->
                    ( inputPath
                    , Internal.deriveFile "inputs" hash <| \_ target ->
                    BackendTask.File.Extra.copyFile
                        { from = inputPath
                        , to = target
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
        { files : PathDict Path.Relative Path.FileOrDirectory FileOrDirectory
        , directories : PathDict Path.Relative Path.FileOrDirectory Tree
        }


{-| -}
combineInto : List { filename : Path Path.Relative Path.File, hash : FileOrDirectory } -> BuildTask e FileOrDirectory
combineInto files =
    files
        |> buildTree
        |> andThen combineTree


buildTree : List { filename : Path Path.Relative Path.File, hash : FileOrDirectory } -> BuildTask e Tree
buildTree files =
    let
        emptyTree : Tree
        emptyTree =
            Tree { files = PathDict.empty, directories = PathDict.empty }

        addFile : { filename : Path Path.Relative Path.File, hash : FileOrDirectory } -> Tree -> Tree
        addFile file tree =
            let
                dir : List (Path Path.Relative Path.Directory)
                dir =
                    file.filename
                        |> Path.parent
                        |> Path.splitDirectory

                filename : Path Path.Relative Path.File
                filename =
                    Path.filename file.filename
            in
            addFile_ dir filename file.hash tree

        addFile_ : List (Path Path.Relative Path.Directory) -> Path Path.Relative Path.File -> FileOrDirectory -> Tree -> Tree
        addFile_ dir filename hash (Tree tree) =
            case dir of
                [] ->
                    Tree
                        { files =
                            PathDict.insert
                                (Path.toFileOrDirectory filename)
                                hash
                                tree.files
                        , directories = tree.directories
                        }

                head :: tail ->
                    Tree
                        { files = tree.files
                        , directories =
                            PathDict.update
                                (Path.toFileOrDirectory head)
                                (\found ->
                                    found
                                        |> Maybe.withDefault emptyTree
                                        |> addFile_ tail filename hash
                                        |> Just
                                )
                                tree.directories
                        }

        result : Tree
        result =
            List.foldl addFile emptyTree files
    in
    isDebug
        |> andThen
            (\debug ->
                if debug then
                    dumpTree result

                else
                    succeed result
            )


dumpTree : Tree -> BuildTask e Tree
dumpTree tree =
    triggerDebugger
        |> andThen
            (\_ ->
                dumpTreeHelp "-" tree
            )


dumpTreeHelp : String -> Tree -> BuildTask e Tree
dumpTreeHelp prefix (Tree tree) =
    let
        directoriesLogs : List (BuildTask e ())
        directoriesLogs =
            tree.directories
                |> PathDict.toList
                |> List.map
                    (\( directory, child ) ->
                        Internal.debugLog (prefix ++ Path.toString directory)
                            |> andThen (\_ -> dumpTreeHelp (prefix ++ "-") child)
                            |> map (\_ -> ())
                    )

        filesLogs : String
        filesLogs =
            tree.files
                |> PathDict.toList
                |> List.map (\( file, _ ) -> Path.toString file)
                |> String.join ", "
    in
    (directoriesLogs ++ [ Internal.debugLog (prefix ++ filesLogs) ])
        |> Internal.combineBy 1
        |> map (\_ -> Tree tree)


isDebug : BuildTask e Bool
isDebug =
    Internal.isDebug


combineTree : Tree -> BuildTask e FileOrDirectory
combineTree (Tree tree) =
    do jobs <| \parallelism ->
    do
        (tree.directories
            |> PathDict.foldl
                (\filename subtree acc ->
                    map (Tuple.pair filename) (combineTree subtree) :: acc
                )
                []
            |> combineBy parallelism
            |> map PathDict.fromList
        )
    <| \subtrees ->
    let
        combined : PathDict Path.Relative Path.FileOrDirectory FileOrDirectory
        combined =
            PathDict.union tree.files subtrees

        outputHash : BuildTask e FileOrDirectory
        outputHash =
            PathDict.foldl
                (\filename hash acc -> Path.toString filename :: Hash.toString hash :: acc)
                [ "combineTree" ]
                combined
                |> String.join "|"
                |> Internal.hashFromString
    in
    do outputHash <| \combinedHash ->
    Internal.deriveDirectory "combine" combinedHash <| \({ buildPath } as input_) target ->
    Do.do
        (BackendTask.File.Extra.makeDirectory { recursive = True } target
            |> BackendTask.mapError Internal.InternalError
        )
    <| \_ ->
    combined
        |> PathDict.foldl
            (\outputFilename hash acc ->
                Internal.execLog input_
                    "cp"
                    [ "-rl"
                    , Path.toString (Hash.toFilePath buildPath hash)
                    , Path.toString (Path.append target outputFilename)
                    ]
                    :: acc
            )
            []
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
    mapRecoverableError e x |> andThen f


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
    Internal.deriveFile "writeFile" hash <| \_ target ->
    BackendTask.File.Extra.write { path = target, body = content }
        |> BackendTask.mapError Internal.UserError


writeBytes : Bytes -> BuildTask FatalError FileOrDirectory
writeBytes content =
    case Hash.fromChecksum (SHA256.toHex (SHA256.fromBytes content)) of
        Err e ->
            Internal.fail (FatalError.fromString e) |> Internal.fatalToInternal

        Ok hash ->
            Internal.deriveFile "writeBytes" hash <| \_ target ->
            BackendTask.File.Extra.writeBinary { path = target, body = content }
                |> BackendTask.mapError Internal.UserError


{-| -}
run :
    { jobs : Maybe Int
    , debug : Bool
    , check : Bool
    , hashKind : Hash.Kind
    , keepFailed : Bool
    }
    -> Path Path.Absolute Path.Directory
    -> BuildTask e FileOrDirectory
    ->
        BackendTask
            (Error e)
            { output : Path Path.Absolute Path.FileOrDirectory
            , intermediate : List (Path Path.Absolute Path.FileOrDirectory)
            , warnings : Set Warning
            }
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


readFromDirectory : FileOrDirectory -> String -> BuildTask { fatal : FatalError, recoverable : File.FileReadError e } String
readFromDirectory directory file =
    do (Internal.extractFromDirectory directory file) <| \extracted ->
    withFile extracted succeed


withMemoryLimitInGB : Int -> BuildTask e a -> BuildTask e a
withMemoryLimitInGB limit task =
    Internal.withMemoryLimitInBytes (limit * 1024 * 1024 * 1024) task


withMemoryLimitInMB : Int -> BuildTask e a -> BuildTask e a
withMemoryLimitInMB limit task =
    Internal.withMemoryLimitInBytes (limit * 1024 * 1024) task


withDebug : (String -> Never) -> BuildTask e a -> BuildTask e a
withDebug todo task =
    Internal.withDebug todo task


withIdlePriority : BuildTask e a -> BuildTask e a
withIdlePriority task =
    Internal.withIdlePriority task


which : String -> BuildTask FatalError Command
which name =
    Internal.which name
