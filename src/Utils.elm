module Utils exposing (escape, viewEnv)

import FastDict as Dict exposing (Dict)
import Json.Encode


escape : String -> String
escape s =
    Json.Encode.encode 0 (Json.Encode.string s)


viewEnv : Dict String String -> String
viewEnv env =
    env
        |> Dict.toList
        |> List.map (\( k, v ) -> k ++ "=" ++ v)
        |> String.join " "
