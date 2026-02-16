module BackendTask.Extra exposing (combineBy, combineBy_, profiling, timed)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.Time
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import List.Extra
import Pages.Script.Spinner as Spinner
import Time


timed : String -> String -> BackendTask error a -> BackendTask error a
timed labelBefore labelAfter task =
    if labelBefore == "DISABLED FOR NOW" then
        let
            options : Spinner.Options error ( a, Int )
            options =
                Spinner.options labelBefore
                    |> Spinner.withOnCompletion
                        (\res ->
                            case res of
                                Err _ ->
                                    ( Spinner.Fail, Nothing )

                                Ok ( _, delta ) ->
                                    let
                                        msg : String
                                        msg =
                                            labelAfter ++ " in " ++ String.fromInt delta ++ "ms"
                                    in
                                    ( Spinner.Succeed, Just msg )
                        )

            inner : BackendTask error ( a, Int )
            inner =
                Do.do BackendTask.Time.now <| \before ->
                Do.do task <| \res ->
                Do.do BackendTask.Time.now <| \after ->
                let
                    delta : Int
                    delta =
                        Time.posixToMillis after - Time.posixToMillis before
                in
                BackendTask.succeed ( res, delta )
        in
        Spinner.runTaskWithOptions options inner
            |> BackendTask.map Tuple.first

    else
        Do.do BackendTask.Time.now <| \before ->
        Do.log labelBefore <| \_ ->
        Do.do task <| \res ->
        Do.do BackendTask.Time.now <| \after ->
        let
            delta : Int
            delta =
                Time.posixToMillis after - Time.posixToMillis before
        in
        Do.log (labelAfter ++ " in " ++ String.fromInt delta ++ "ms") <| \_ ->
        BackendTask.succeed res


combineBy :
    Int
    -> List (BackendTask error a)
    -> BackendTask error (List a)
combineBy n list =
    list
        |> List.Extra.greedyGroupsOf n
        |> List.map BackendTask.combine
        |> BackendTask.sequence
        |> BackendTask.map List.concat


combineBy_ :
    Int
    -> List (BackendTask error ())
    -> BackendTask error ()
combineBy_ n list =
    list
        |> List.Extra.greedyGroupsOf n
        |> List.map BackendTask.combine
        |> sequenceIgnore


sequenceIgnore : List (BackendTask error (List ())) -> BackendTask error ()
sequenceIgnore inputs =
    List.foldr (\e acc -> acc |> BackendTask.andThen (\_ -> e)) (BackendTask.succeed []) inputs
        |> BackendTask.map (\_ -> ())


profiling : String -> BackendTask FatalError a -> BackendTask FatalError a
profiling label t =
    BackendTask.Custom.run "profile" (Json.Encode.string label) (Json.Decode.succeed ())
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\() ->
                t
            )
        |> BackendTask.andThen
            (\res ->
                BackendTask.Custom.run "profileEnd" (Json.Encode.string label) (Json.Decode.succeed ())
                    |> BackendTask.allowFatal
                    |> BackendTask.map (\() -> res)
            )
