module HashSet exposing (HashSet, empty, equals, fromList, insert, member, toList, union)

import BST exposing (BST)
import Hash exposing (Hash)


type HashSet
    = HashSet (BST String)


empty : HashSet
empty =
    HashSet BST.empty


member : Hash -> HashSet -> Bool
member x (HashSet s) =
    BST.member (Hash.toString x) s


insert : Hash -> HashSet -> HashSet
insert x (HashSet s) =
    HashSet (BST.insert (Hash.toString x) s)


union : HashSet -> HashSet -> HashSet
union (HashSet a) (HashSet b) =
    HashSet (BST.union a b)


toList : HashSet -> List Hash
toList (HashSet s) =
    BST.toList s
        |> List.map Hash.unsafe


fromList : List String -> HashSet
fromList list =
    list
        |> BST.fromList
        |> HashSet


equals : HashSet -> HashSet -> Bool
equals (HashSet l) (HashSet r) =
    BST.equals l r
