module BackendTask.Extra exposing (combineMap, log, pwd)

import Ansi.Color as Color
import BackendTask exposing (BackendTask)
import BackendTask.Env
import FatalError exposing (FatalError)
import Pages.Script as Script


pwd : BackendTask FatalError (Maybe String)
pwd =
    BackendTask.Env.get "PWD"


log : Color.Color -> String -> (a -> String) -> BackendTask FatalError a -> BackendTask FatalError a
log color label toString task =
    task
        |> BackendTask.andThen
            (\value ->
                Script.log (Color.fontColor color (label ++ ":") ++ " " ++ toString value)
                    |> BackendTask.map (\_ -> value)
            )


combineMap : (a -> BackendTask FatalError b) -> List a -> BackendTask FatalError (List b)
combineMap f args =
    BackendTask.combine <| List.map f args
