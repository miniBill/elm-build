module Hash exposing (FileType(..), Hash, Kind(..), fromChecksum, fromString, toPath, toString, toTmp, toWorkspace, unsafe)

import BST exposing (BST)
import FNV1a
import Hex
import Path exposing (Path)
import Sha256


type
    Hash
    --FileType Int
    = Hash String


type Kind
    = Fast
    | Secure


type FileType
    = Normal
    | Temporary



-- toTuple (Hash type_ i _) =
--     ( type_, i )


{-| Build an hashed file from the output of shaXsum/b3sum.
-}
fromChecksum : String -> Result String Hash
fromChecksum raw =
    let
        clean : String
        clean =
            String.left 8 raw
    in
    Hex.fromString clean
        |> Result.map (\_ -> Hash clean)


{-| Build a hash directly without checking that it's a valid hex string.
-}
unsafe : String -> Hash
unsafe h =
    Hash h


toPath : Path -> Hash -> String
toPath buildPath (Hash hash) =
    Path.toString buildPath ++ "/" ++ hash


toString : Hash -> String
toString (Hash hash) =
    hash


toTmp : Hash -> Hash
toTmp (Hash hash) =
    Hash ("tmp-" ++ hash)


toWorkspace : Hash -> Hash
toWorkspace (Hash hash) =
    Hash ("workspace-" ++ hash)


{-| Build a hash from an arbitrary string.
-}
fromString : String -> Kind -> Hash
fromString raw hashKind =
    case hashKind of
        Fast ->
            raw
                |> FNV1a.hash
                |> Hex.toString
                |> String.padLeft 8 '0'
                |> Hash

        Secure ->
            Sha256.sha256 raw
                |> Hash
