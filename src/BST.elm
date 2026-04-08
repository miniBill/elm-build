module BST exposing (BST, empty, equals, fromList, insert, member, toList, union, unionAll)

import Array exposing (Array)


type BST a
    = BSTNode a (BST a) (BST a)
    | BSTLeaf


member : comparable -> BST comparable -> Bool
member k t =
    case t of
        BSTLeaf ->
            False

        BSTNode nk l r ->
            case compare k nk of
                EQ ->
                    True

                LT ->
                    member k l

                GT ->
                    member k r


empty : BST a
empty =
    BSTLeaf


insert : comparable -> BST comparable -> BST comparable
insert k t =
    case insertHelp k t of
        Nothing ->
            t

        Just newT ->
            newT


insertHelp : comparable -> BST comparable -> Maybe (BST comparable)
insertHelp k t =
    case t of
        BSTLeaf ->
            Just (BSTNode k BSTLeaf BSTLeaf)

        BSTNode nk l r ->
            case compare k nk of
                EQ ->
                    Nothing

                LT ->
                    case insertHelp k l of
                        Nothing ->
                            Nothing

                        Just nl ->
                            Just (BSTNode nk nl r)

                GT ->
                    case insertHelp k r of
                        Nothing ->
                            Nothing

                        Just nr ->
                            Just (BSTNode nk l nr)


union : BST comparable -> BST comparable -> BST comparable
union l r =
    unionHelp 0 l r


unionHelp : Int -> BST comparable -> BST comparable -> BST comparable
unionHelp budget l r =
    if budget <= 200 then
        fromSortedList (mergeSorted (toList l) (toList r) [])

    else
        case l of
            BSTLeaf ->
                r

            BSTNode lk ll lr ->
                let
                    ( rl, rr ) =
                        split lk r
                in
                BSTNode lk (unionHelp (budget - 1) ll rl) (unionHelp (budget - 1) lr rr)


mergeSorted : List comparable -> List comparable -> List comparable -> List comparable
mergeSorted l r acc =
    case l of
        [] ->
            List.reverse (List.reverse r ++ acc)

        lh :: lt ->
            case r of
                [] ->
                    List.reverse (List.reverse l ++ acc)

                rh :: rt ->
                    case compare lh rh of
                        EQ ->
                            mergeSorted lt rt (lh :: acc)

                        LT ->
                            mergeSorted lt r (lh :: acc)

                        GT ->
                            mergeSorted l rt (rh :: acc)


split : comparable -> BST comparable -> ( BST comparable, BST comparable )
split k t =
    case t of
        BSTLeaf ->
            ( BSTLeaf, BSTLeaf )

        BSTNode nk l r ->
            case compare k nk of
                EQ ->
                    ( l, r )

                LT ->
                    let
                        ( ll, lr ) =
                            split k l
                    in
                    ( ll, BSTNode k lr r )

                GT ->
                    let
                        ( rl, rr ) =
                            split k r
                    in
                    ( BSTNode k l rl, rr )


toList : BST a -> List a
toList s =
    toListHelp s []


toListHelp : BST a -> List a -> List a
toListHelp t acc =
    case t of
        BSTLeaf ->
            acc

        BSTNode k l r ->
            toListHelp r acc
                |> (::) k
                |> toListHelp l


fromList : List comparable -> BST comparable
fromList list =
    fromSortedList (List.sort list)


fromSortedList : List a -> BST a
fromSortedList list =
    let
        arr : Array a
        arr =
            list
                |> unique
                |> Array.fromList
    in
    fromSortedListHelp 0 (Array.length arr) arr


fromSortedListHelp : Int -> Int -> Array a -> BST a
fromSortedListHelp fromIncluded toExcluded arr =
    if fromIncluded >= toExcluded then
        BSTLeaf

    else
        let
            mid : Int
            mid =
                fromIncluded + (toExcluded - fromIncluded) // 2
        in
        case Array.get mid arr of
            Nothing ->
                BSTLeaf

            Just k ->
                BSTNode k
                    (fromSortedListHelp fromIncluded mid arr)
                    (fromSortedListHelp (mid + 1) toExcluded arr)


{-| Remove duplicate elements as long as they're next to each other.
-}
unique : List a -> List a
unique list =
    case list of
        [] ->
            []

        h :: t ->
            let
                ( nh, nt ) =
                    List.foldl
                        (\e ( l, a ) ->
                            if e == l then
                                ( l, a )

                            else
                                ( e, l :: a )
                        )
                        ( h, [] )
                        t
            in
            List.reverse (nh :: nt)


equals : BST comparable -> BST comparable -> Bool
equals l r =
    equalsHelp [ l ] [ r ]


equalsHelp : List (BST a) -> List (BST a) -> Bool
equalsHelp lq rq =
    case lq of
        BSTLeaf :: lt ->
            equalsHelp lt rq

        [] ->
            case rq of
                BSTLeaf :: rt ->
                    equalsHelp lq rt

                [] ->
                    True

                _ ->
                    False

        (BSTNode lv ll lr) :: lt ->
            case rq of
                BSTLeaf :: rt ->
                    equalsHelp lq rt

                [] ->
                    False

                (BSTNode rv rl rr) :: rt ->
                    case ( ll, rl ) of
                        ( BSTLeaf, BSTLeaf ) ->
                            if lv == rv then
                                equalsHelp (lr :: lt) (rr :: rt)

                            else
                                False

                        ( BSTLeaf, BSTNode _ _ _ ) ->
                            equalsHelp lq (rl :: BSTNode rv BSTLeaf rr :: rt)

                        ( BSTNode _ _ _, BSTLeaf ) ->
                            equalsHelp (ll :: BSTNode lv BSTLeaf lr :: lt) rq

                        ( BSTNode _ _ _, BSTNode _ _ _ ) ->
                            equalsHelp (ll :: BSTNode lv BSTLeaf lr :: lt) (rl :: BSTNode rv BSTLeaf rr :: rt)


unionAll : List (BST comparable) -> BST comparable
unionAll list =
    let
        array : Array (BST comparable)
        array =
            Array.fromList list
    in
    unionAllHelp 0 (Array.length array) array


unionAllHelp : Int -> Int -> Array (BST comparable) -> BST comparable
unionAllHelp from to arr =
    if to < from then
        empty

    else if to == from + 1 then
        Array.get from arr |> Maybe.withDefault empty

    else
        let
            mid : Int
            mid =
                from + (to - from) // 2

            l : BST comparable
            l =
                unionAllHelp from mid arr

            r : BST comparable
            r =
                unionAllHelp mid to arr
        in
        union l r
