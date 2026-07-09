module HashSet exposing (HashSet, empty, equals, fromList, insert, member, toList, union, unionAll)

import BST exposing (BST)
import Hash exposing (Hash, Normal)


{-| Assumption: this will only contain normal hashes.
-}
type HashSet
    = HashSet (BST String)


empty : HashSet
empty =
    HashSet BST.empty


member : Hash Normal -> HashSet -> Bool
member x (HashSet s) =
    BST.member (Hash.toString x) s


insert : Hash Normal -> HashSet -> HashSet
insert x (HashSet s) =
    HashSet (BST.insert (Hash.toString x) s)


union : HashSet -> HashSet -> HashSet
union (HashSet a) (HashSet b) =
    HashSet (BST.union a b)


toList : HashSet -> List (Hash Normal)
toList (HashSet s) =
    BST.toList s
        |> List.map Hash.build


fromList : List String -> Result String HashSet
fromList list =
    list
        |> BST.fromList
        |> HashSet
        |> Ok


equals : HashSet -> HashSet -> Bool
equals (HashSet l) (HashSet r) =
    BST.equals l r


unionAll : List HashSet -> HashSet
unionAll sets =
    sets
        |> List.map (\(HashSet h) -> h)
        |> BST.unionAll
        |> HashSet
