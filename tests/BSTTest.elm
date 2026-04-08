module BSTTest exposing (equal, toFromList)

import BST exposing (BST)
import Expect
import Fuzz exposing (Fuzzer)
import Set
import Test exposing (Test)


toFromList : Test
toFromList =
    Test.describe "toList/fromList"
        [ Test.fuzz (Fuzz.listOfLengthBetween 0 100 Fuzz.int) "fromList >> toList == identity" <| \list ->
        list
            |> BST.fromList
            |> BST.toList
            |> Expect.equal (Set.toList (Set.fromList list))
        , Test.fuzz (bstFuzzer 100 Fuzz.int) "toList >> fromList == identity" <| \bst ->
        let
            rebuilt : BST Int
            rebuilt =
                bst
                    |> BST.toList
                    |> BST.fromList
        in
        rebuilt
            |> BST.equals bst
            |> Expect.equal True
            |> Expect.onFail
                ("Expected: "
                    ++ Debug.toString bst
                    ++ "\n  Actual: "
                    ++ Debug.toString rebuilt
                )
        ]


equal : Test
equal =
    Test.describe "BST.equal"
        [ Test.fuzz (bstFuzzer 100 Fuzz.int) "a == a" <| \a ->
        a
            |> BST.equals a
            |> Expect.equal True
        , Test.test "[0, 1] == [0, 1]" <| \_ ->
        let
            bst : BST Int
            bst =
                BST.fromList [ 0, 1 ]
        in
        bst |> BST.equals bst |> Expect.equal True
        ]


bstFuzzer : Int -> Fuzzer comparable -> Fuzzer (BST comparable)
bstFuzzer budget fuzzer =
    if budget <= 0 then
        Fuzz.constant BST.empty

    else
        Fuzz.oneOf
            [ fromListFuzzer budget fuzzer
            , fromOpsFuzzer budget fuzzer
            ]


fromOpsFuzzer : Int -> Fuzzer comparable -> Fuzzer (BST comparable)
fromOpsFuzzer budget fuzzer =
    opFuzzer budget fuzzer
        |> Fuzz.listOfLengthBetween 0 budget
        |> Fuzz.map (List.foldl applyOp BST.empty)


type Op a
    = Insert a
    | UnionL (BST a)
    | UnionR (BST a)


opFuzzer : Int -> Fuzzer comparable -> Fuzzer (Op comparable)
opFuzzer budget fuzzer =
    Fuzz.oneOf
        [ Fuzz.map Insert fuzzer
        , Fuzz.map UnionL (bstFuzzer (budget // 10) fuzzer)
        , Fuzz.map UnionR (bstFuzzer (budget // 10) fuzzer)
        ]


applyOp : Op comparable -> BST comparable -> BST comparable
applyOp op bst =
    case op of
        Insert v ->
            BST.insert v bst

        UnionL l ->
            BST.union l bst

        UnionR r ->
            BST.union bst r


fromListFuzzer : Int -> Fuzzer comparable -> Fuzzer (BST comparable)
fromListFuzzer budget fuzzer =
    fuzzer
        |> Fuzz.listOfLengthBetween 0 budget
        |> Fuzz.map BST.fromList
