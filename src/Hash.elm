module Hash exposing (Hash, Kind(..), Normal, Temporary, Workspace, build, fromChecksum, fromString, toDirectoryPath, toFileOrDirectoryPath, toFilePath, toPathWorkspace, toString, toTemporary, toTemporaryDirectory, toTemporaryFile, toWorkspace)

import FNV1a
import Hex
import Path.Posix as Path exposing (Directory, File, FileOrDirectory, Path)
import SHA256
import Utils


type Hash k
    = Hash String


type Kind
    = Fast
    | Secure


type Normal
    = Normal


type Temporary
    = Temporary


type Workspace
    = Workspace


{-| Build an hashed file from the output of shaXsum/b3sum.
-}
fromChecksum : String -> Result String (Hash Normal)
fromChecksum raw =
    let
        clean : String
        clean =
            raw
                |> String.toLower
                |> String.left 8
    in
    if
        String.all
            (\char ->
                let
                    code : Int
                    code =
                        Char.toCode char
                in
                -- 0-9
                (0x30 <= code && code <= 0x39)
                    -- a-f
                    || (0x61 <= code && code <= 0x66)
            )
            clean
    then
        Ok (Hash clean)

    else
        Err ("Invalid hex string: " ++ Utils.escape clean)


build : String -> Hash Normal
build i =
    Hash i


toDirectoryPath : Path base Directory -> Hash Normal -> Path base Directory
toDirectoryPath buildPath hash =
    Path.append
        buildPath
        (Path.parseRelativeDirectory (toString hash)
            |> -- hash is guaranteed to contain 0-9a-f, therefore the above will be valid
               trustMe
        )


toFileOrDirectoryPath : Path base Directory -> Hash Normal -> Path base FileOrDirectory
toFileOrDirectoryPath buildPath hash =
    Path.append
        buildPath
        (Path.parseRelativeFileOrDirectory (toString hash)
            |> -- hash is guaranteed to contain 0-9a-f, therefore the above will be valid
               trustMe
        )


toFilePath : Path base Directory -> Hash Normal -> Path base File
toFilePath buildPath hash =
    Path.append
        buildPath
        (Path.parseRelativeFile (toString hash)
            |> -- hash is guaranteed to contain 0-9a-f, therefore the above will be valid
               trustMe
        )


toTemporaryDirectory : Path base Directory -> Hash Temporary -> Path base Directory
toTemporaryDirectory buildPath hash =
    Path.append
        buildPath
        (Path.parseRelativeDirectory ("tmp-" ++ toString hash)
            |> -- hash is guaranteed to contain 0-9a-f, therefore the above will be valid
               trustMe
        )


toTemporaryFile : Path base Directory -> Hash Temporary -> Path base File
toTemporaryFile buildPath hash =
    Path.append
        buildPath
        (Path.parseRelativeFile ("tmp-" ++ toString hash)
            |> -- hash is guaranteed to contain 0-9a-f, therefore the above will be valid
               trustMe
        )


trustMe : Result e v -> v
trustMe res =
    case res of
        Err _ ->
            let
                _ =
                    -- Crash
                    modBy 0 0
            in
            trustMe res

        Ok v ->
            v


toPathWorkspace : Path base Directory -> Hash Workspace -> Path base Directory
toPathWorkspace buildPath hash =
    Path.append
        buildPath
        (Path.parseRelativeDirectory ("workspace-" ++ toString hash)
            |> -- hash is guaranteed to contain 0-9a-f, therefore the above will be valid
               trustMe
        )


toString : Hash k -> String
toString (Hash hash) =
    String.padLeft 8 '0' hash


toTemporary : Hash Normal -> Hash Temporary
toTemporary (Hash hash) =
    Hash hash


toWorkspace : Hash Normal -> Hash Workspace
toWorkspace (Hash hash) =
    Hash hash


{-| Build a hash from an arbitrary string.
-}
fromString : String -> Kind -> Hash Normal
fromString raw hashKind =
    case hashKind of
        Fast ->
            raw
                |> FNV1a.hash
                |> Hex.fromWord32
                |> Hash

        Secure ->
            raw
                |> SHA256.fromString
                |> SHA256.toHex
                |> Hash
