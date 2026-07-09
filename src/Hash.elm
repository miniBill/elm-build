module Hash exposing (Hash, Kind(..), Normal, Temporary, Workspace, build, fromChecksum, fromString, toPath, toPathTemporary, toPathWorkspace, toString, toTemporary, toWorkspace)

import FNV1a
import Hex
import Path exposing (Path)
import SHA256


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
            String.left 8 raw
    in
    Ok (Hash clean)


build : String -> Hash Normal
build i =
    Hash i


toPath : Path -> Hash Normal -> String
toPath buildPath hash =
    Path.toString buildPath ++ "/" ++ toString hash


toPathTemporary : Path -> Hash Temporary -> String
toPathTemporary buildPath hash =
    Path.toString buildPath ++ "/tmp-" ++ toString hash


toPathWorkspace : Path -> Hash Workspace -> String
toPathWorkspace buildPath hash =
    Path.toString buildPath ++ "/workspace-" ++ toString hash


toString : Hash k -> String
toString (Hash hash) =
    String.padLeft 8 '0' hash


toTemporary : Hash Normal -> Hash Temporary
toTemporary (Hash hash) =
    Hash hash


toWorkspace : Hash Temporary -> Hash Workspace
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
