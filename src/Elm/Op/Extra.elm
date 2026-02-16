module Elm.Op.Extra exposing (appends)

import Elm
import Elm.Op


appends : Elm.Expression -> List Elm.Expression -> Elm.Expression
appends first list =
    List.foldl (\e a -> Elm.Op.append a e) first list
