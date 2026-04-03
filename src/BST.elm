module BST exposing (BST, empty, equals, fromList, insert, member, toList, union)

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
    case l of
        BSTLeaf ->
            r

        BSTNode lk ll lr ->
            let
                ( rl, rr ) =
                    split lk r
            in
            BSTNode lk (union ll rl) (union lr rr)


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
                    ( ll, BSTNode nk lr r )

                GT ->
                    let
                        ( rl, rr ) =
                            split k r
                    in
                    ( BSTNode nk l rl, rr )


toList : BST a -> List a
toList s =
    let
        go : BST a -> List a -> List a
        go t acc =
            case t of
                BSTLeaf ->
                    acc

                BSTNode k l r ->
                    go r acc
                        |> (::) k
                        |> go l
    in
    go s []


fromList : List comparable -> BST comparable
fromList list =
    let
        arr : Array comparable
        arr =
            List.sort list
                |> unique
                |> Array.fromList

        go fromIncluded toExcluded =
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
                        BSTNode k (go fromIncluded mid) (go (mid + 1) toExcluded)
    in
    go 0 (Array.length arr)


unique : List comparable -> List comparable
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
    let
        go : List (BST comparable) -> List (BST comparable) -> Bool
        go lq rq =
            case ( lq, rq ) of
                ( BSTLeaf :: lt, _ ) ->
                    go lt rq

                ( _, BSTLeaf :: rt ) ->
                    go lq rt

                ( [], [] ) ->
                    True

                ( [], _ ) ->
                    False

                ( _, [] ) ->
                    False

                ( (BSTNode lv BSTLeaf lr) :: lt, (BSTNode rv BSTLeaf rr) :: rt ) ->
                    if lv == rv then
                        go (lr :: lt) (rr :: rt)

                    else
                        False

                ( (BSTNode lv BSTLeaf lr) :: lt, (BSTNode rv ((BSTNode _ _ _) as rl) rr) :: rt ) ->
                    go lq (rl :: BSTNode rv BSTLeaf rr :: rt)

                ( (BSTNode lv ((BSTNode _ _ _) as ll) lr) :: lt, (BSTNode rv BSTLeaf rr) :: rt ) ->
                    go (ll :: BSTNode lv BSTLeaf lr :: lt) rq

                ( (BSTNode lv ((BSTNode _ _ _) as ll) lr) :: lt, (BSTNode rv ((BSTNode _ _ _) as rl) rr) :: rt ) ->
                    go (ll :: BSTNode lv BSTLeaf lr :: lt) (rl :: BSTNode rv BSTLeaf rr :: rt)
    in
    go [ l ] [ r ]
