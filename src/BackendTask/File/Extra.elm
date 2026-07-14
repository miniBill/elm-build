module BackendTask.File.Extra exposing (copyFile, exists, listDirectoriesIn, listFilesAndDirectoriesIn, listFilesIn, makeDirectory, move, read, removeIfExists, resolveDirectory, resolveFile, write, writeBinary)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.File as File
import Bytes exposing (Bytes)
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import Pages.Script as Script
import Path.Posix as Path exposing (Absolute, Directory, File, Path, Relative)
import XBytes


read : Path base File -> BackendTask { fatal : FatalError, recoverable : File.FileReadError decoderError } String
read file =
    File.rawFile (Path.toString file)


move : { from : Path base kind, to : Path base2 kind } -> BackendTask FatalError ()
move { from, to } =
    Script.move
        { from = Path.toString from
        , to = Path.toString to
        }


copyFile : { from : Path base File, to : Path base2 File } -> BackendTask FatalError ()
copyFile { from, to } =
    Script.copyFile
        { from = Path.toString from
        , to = Path.toString to
        }


removeIfExists : Path base fileOrDirectory -> BackendTask FatalError ()
removeIfExists filename =
    Do.do (exists filename) <| \exists_ ->
    if exists_ then
        Do.exec "chmod" [ "-R", "700", Path.toString filename ] <| \_ ->
        Script.exec "rm" [ "-r", Path.toString filename ]

    else
        Do.noop


exists : Path base kind -> BackendTask FatalError Bool
exists path =
    File.exists (Path.toString path)


write : { path : Path base File, body : String } -> BackendTask { fatal : FatalError, recoverable : Script.Error } ()
write { path, body } =
    Script.writeFile { path = Path.toString path, body = body }


writeBinary : { path : Path base File, body : Bytes } -> BackendTask FatalError ()
writeBinary { path, body } =
    BackendTask.Custom.run "writeBinaryFile"
        (Json.Encode.object
            [ ( "path", Json.Encode.string (Path.toString path) )
            , ( "body", Json.Encode.string (XBytes.toHex (XBytes.fromBytes body)) )
            ]
        )
        (Json.Decode.succeed ())
        |> BackendTask.allowFatal


resolveFile : String -> BackendTask FatalError (Path Absolute File)
resolveFile path =
    resolve path
        |> BackendTask.andThen
            (\raw ->
                case Path.parseAbsoluteFile raw of
                    Err e ->
                        BackendTask.fail (FatalError.fromString (Debug.toString e))

                    Ok parsed ->
                        BackendTask.succeed parsed
            )


resolveDirectory : String -> BackendTask FatalError (Path Absolute Directory)
resolveDirectory path =
    resolve path
        |> BackendTask.andThen
            (\raw ->
                case Path.parseAbsoluteDirectory raw of
                    Err e ->
                        BackendTask.fail (FatalError.fromString (Debug.toString e))

                    Ok parsed ->
                        BackendTask.succeed parsed
            )


resolve : String -> BackendTask FatalError String
resolve path =
    BackendTask.Custom.run "resolve"
        (Json.Encode.string path)
        Json.Decode.string
        |> BackendTask.allowFatal


listFilesIn : Path base Directory -> BackendTask FatalError (List (Path Relative File))
listFilesIn path =
    listFilesAndDirectoriesIn path |> BackendTask.map Tuple.first


listDirectoriesIn : Path base Directory -> BackendTask FatalError (List (Path Relative Directory))
listDirectoriesIn path =
    listFilesAndDirectoriesIn path |> BackendTask.map Tuple.second


listFilesAndDirectoriesIn :
    Path base Directory
    ->
        BackendTask
            FatalError
            ( List (Path Relative File)
            , List (Path Relative Directory)
            )
listFilesAndDirectoriesIn path =
    let
        decodeListAt :
            String
            -> (String -> Result error (Path Relative kind))
            -> Json.Decode.Decoder (List (Path Relative kind))
        decodeListAt field inner =
            Json.Decode.string
                |> Json.Decode.andThen
                    (\file ->
                        case inner file of
                            Err e ->
                                Json.Decode.fail (Debug.toString e)

                            Ok parsed ->
                                Json.Decode.succeed parsed
                    )
                |> Json.Decode.list
                |> Json.Decode.field field
    in
    BackendTask.Custom.run "readdir"
        (Json.Encode.string (Path.toString path))
        (Json.Decode.map2 Tuple.pair
            (decodeListAt "files" Path.parseRelativeFile)
            (decodeListAt "directories" Path.parseRelativeDirectory)
        )
        |> BackendTask.allowFatal


makeDirectory : { recursive : Bool } -> Path base Directory -> BackendTask FatalError ()
makeDirectory opts path =
    Script.makeDirectory opts (Path.toString path)
