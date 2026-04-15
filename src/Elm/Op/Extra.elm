module Elm.Op.Extra exposing (appendStrings)

import Elm
import Elm.Op


appendStrings : List Elm.Expression -> Elm.Expression
appendStrings list =
    case list of
        [] ->
            Elm.string ""

        [ x ] ->
            x

        h :: t ->
            List.foldl (\e a -> Elm.Op.append a e) h t
