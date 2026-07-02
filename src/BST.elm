module BST exposing (BST, empty, equals, fromList, insert, member, toList, union, unionAll)

import Array exposing (Array)
import FastSet as Set exposing (Set)


type BST a
    = BST (Set a)


member : comparable -> BST comparable -> Bool
member k (BST t) =
    Set.member k t


empty : BST a
empty =
    BST Set.empty


insert : comparable -> BST comparable -> BST comparable
insert k (BST t) =
    BST (Set.insert k t)


union : BST comparable -> BST comparable -> BST comparable
union (BST l) (BST r) =
    BST (Set.union l r)


toList : BST a -> List a
toList (BST s) =
    Set.toList s


fromList : List comparable -> BST comparable
fromList list =
    BST (Set.fromList list)


equals : BST comparable -> BST comparable -> Bool
equals (BST l) (BST r) =
    Set.equals l r


unionAll : List (BST comparable) -> BST comparable
unionAll list =
    unionAllFirstStage [] list


unionAllFirstStage : List (List comparable) -> List (BST comparable) -> BST comparable
unionAllFirstStage acc queue =
    case queue of
        [] ->
            unionAllHelp [] acc

        [ x ] ->
            unionAllFirstStage (toList x :: acc) []

        first :: second :: rest ->
            unionAllFirstStage (mergeSorted (toList first) (toList second) [] :: acc) rest


unionAllHelp : List (List comparable) -> List (List comparable) -> BST comparable
unionAllHelp acc queue =
    case queue of
        [] ->
            case acc of
                [] ->
                    empty

                [ x ] ->
                    fromSortedList x

                _ :: _ :: _ ->
                    unionAllHelp [] acc

        [ x ] ->
            unionAllHelp (x :: acc) []

        first :: second :: rest ->
            unionAllHelp (mergeSorted first second [] :: acc) rest
