module Cache.Internal exposing (Hash, HashSet, Monad(..), derive, extendHashWith, hashSetEmpty, hashSetFromList, hashSetInsert, hashSetMember, hashSetToList, hashSetUnion, hashToPath, hashToString, hashToTmp, hashToWorkspace, inputHash, stringToHash)

import BST exposing (BST)
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import FNV1a
import FatalError exposing (FatalError)
import Hex
import Path exposing (Path)


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


derive :
    -- String ->
    Hash
    ->
        (Input
         -> Hash
         -> BackendTask FatalError ()
        )
    -> Monad Hash
derive {- description -} target inner =
    Monad "derive"
        (\({ existing, buildPath } as input_) deps ->
            let
                newDeps : HashSet
                newDeps =
                    hashSetInsert target deps

                -- appendLog : String -> BackendTask FatalError ()
                -- appendLog line =
                --     BackendTask.Custom.run "appendLog" (Json.Encode.string line) (Json.Decode.succeed ())
                --         |> BackendTask.allowFatal
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
                    tmp : Hash
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



------------
-- HASHES --
------------


{-| -}
type Hash
    = Hash String


{-| Build an hashed file from the output of shaXsum/b3sum.
-}
inputHash : String -> Result String Hash
inputHash raw =
    let
        clean : String
        clean =
            String.left 8 raw
    in
    Hex.fromString clean
        |> Result.map (\_ -> Hash clean)


hashToPath : Path -> Hash -> String
hashToPath buildPath (Hash hash) =
    Path.toString buildPath ++ "/" ++ hash


hashToString : Hash -> String
hashToString (Hash hash) =
    hash


hashToTmp : Hash -> Hash
hashToTmp (Hash hash) =
    Hash ("tmp-" ++ hash)


hashToWorkspace : Hash -> Hash
hashToWorkspace (Hash hash) =
    Hash ("workspace-" ++ hash)


extendHashWith : List String -> Hash -> Hash
extendHashWith l (Hash r) =
    stringToHash (String.join "|" (r :: l))


stringToHash : String -> Hash
stringToHash raw =
    raw
        |> FNV1a.hash
        |> Hex.toString
        |> String.padLeft 8 '0'
        |> Hash


type HashSet
    = HashSet (BST String)


hashSetEmpty : HashSet
hashSetEmpty =
    HashSet BST.empty


hashSetMember : Hash -> HashSet -> Bool
hashSetMember (Hash x) (HashSet s) =
    BST.member x s


hashSetInsert : Hash -> HashSet -> HashSet
hashSetInsert (Hash x) (HashSet s) =
    HashSet (BST.insert x s)


hashSetUnion : HashSet -> HashSet -> HashSet
hashSetUnion (HashSet a) (HashSet b) =
    HashSet (BST.union a b)


hashSetToList : HashSet -> List Hash
hashSetToList (HashSet s) =
    BST.toList s
        |> List.map Hash


hashSetFromList : List String -> HashSet
hashSetFromList list =
    list
        |> BST.fromList
        |> HashSet
