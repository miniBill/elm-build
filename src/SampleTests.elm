module SampleTests exposing (suite)

import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Sample Tests"
        [ describe "Math"
            [ test "addition" <|
                \_ -> Expect.equal (1 + 1) 2
            , test "multiplication" <|
                \_ -> Expect.equal (3 * 4) 12
            , test "negative" <|
                \_ -> Expect.equal (negate 5) -5
            ]
        , describe "Strings"
            [ test "reverse" <|
                \_ -> Expect.equal (String.reverse "hello") "olleh"
            , test "length" <|
                \_ -> Expect.equal (String.length "elm") 3
            , test "concat" <|
                \_ -> Expect.equal (String.append "foo" "bar") "foobar"
            ]
        , describe "Lists"
            [ test "head" <|
                \_ -> Expect.equal (List.head [ 1, 2, 3 ]) (Just 1)
            , test "length" <|
                \_ -> Expect.equal (List.length [ 1, 2, 3 ]) 3
            , test "reverse" <|
                \_ -> Expect.equal (List.reverse [ 1, 2, 3 ]) [ 3, 2, 1 ]
            ]
        , describe "Fuzz"
            [ fuzz Fuzz.int "int identity" <|
                \n -> Expect.equal n n
            , fuzz Fuzz.string "string reverse reverse" <|
                \s -> Expect.equal (String.reverse (String.reverse s)) s
            ]
        , describe "Expensive"
            [ test "large list sort" <|
                \_ ->
                    let
                        big =
                            List.range 1 50000 |> List.reverse
                    in
                    Expect.equal (List.sort big) (List.range 1 50000)
            , test "nested string building" <|
                \_ ->
                    let
                        result =
                            List.range 1 10000
                                |> List.map String.fromInt
                                |> String.join ","
                                |> String.split ","
                                |> List.length
                    in
                    Expect.equal result 10000
            , test "repeated list operations" <|
                \_ ->
                    let
                        result =
                            List.range 1 20000
                                |> List.map (\n -> n * n)
                                |> List.filter (\n -> modBy 3 n == 0)
                                |> List.map String.fromInt
                                |> List.reverse
                                |> List.length
                    in
                    Expect.greaterThan 0 result
            , test "fibonacci-style accumulation" <|
                \_ ->
                    let
                        fib n =
                            fibHelper n 0 1

                        fibHelper n a b =
                            if n <= 0 then
                                a

                            else
                                fibHelper (n - 1) b (a + b)
                    in
                    Expect.greaterThan 0 (fib 10000)
            , fuzz (Fuzz.list Fuzz.int) "sort is idempotent" <|
                \list ->
                    Expect.equal (List.sort list) (List.sort (List.sort list))
            , fuzz (Fuzz.list Fuzz.string) "reverse reverse is identity" <|
                \list ->
                    Expect.equal list (List.reverse (List.reverse list))
            , fuzz (Fuzz.list Fuzz.int) "length preserved after map" <|
                \list ->
                    Expect.equal (List.length list) (List.length (List.map ((*) 2) list))
            , fuzz (Fuzz.list Fuzz.int) "filter length <= original length" <|
                \list ->
                    Expect.atMost (List.length list) (List.length (List.filter (\x -> x > 0) list))
            ]
        ]
