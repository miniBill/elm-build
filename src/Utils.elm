module Utils exposing (escape)

import Json.Encode


escape : String -> String
escape s =
    Json.Encode.encode 0 (Json.Encode.string s)
