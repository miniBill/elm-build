module BackendTask.Extra exposing (combineBy, combineBy_, finally, profiling, sequence_, timed)

import Array exposing (Array)
import BackendTask exposing (BackendTask)
import BackendTask.Customs
import BackendTask.Do as Do
import BackendTask.Time
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script.Spinner as Spinner
import Rope exposing (Rope)
import Time


timed : String -> String -> BackendTask error a -> BackendTask error a
timed labelBefore labelAfter task =
    case labelBefore of
        "DISABLED FOR NOW" ->
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

        _ ->
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
        |> sequence
        |> BackendTask.map List.concat


combineBy_ :
    Int
    -> List (BackendTask error ())
    -> BackendTask error ()
combineBy_ n list =
    list
        |> List.Extra.greedyGroupsOf n
        |> List.map combineIgnore
        |> sequence_
        |> BackendTask.map (\_ -> ())


combineIgnore : List (BackendTask error a) -> BackendTask error ()
combineIgnore inputs =
    let
        arr : Array (BackendTask error a)
        arr =
            Array.fromList inputs

        go : Int -> Int -> BackendTask error ()
        go fromIncluded toExcluded =
            let
                sliceSize : Int
                sliceSize =
                    toExcluded - fromIncluded
            in
            if sliceSize > 128 then
                let
                    mid : Int
                    mid =
                        fromIncluded + sliceSize // 2
                in
                BackendTask.map2 (\_ _ -> ())
                    (go fromIncluded mid)
                    (go mid toExcluded)

            else
                arr
                    |> Array.slice fromIncluded toExcluded
                    |> Array.toList
                    |> BackendTask.combine
                    |> BackendTask.map (\_ -> ())
    in
    go 0 (Array.length arr)


sequence : List (BackendTask error a) -> BackendTask error (List a)
sequence inputs =
    let
        arr : Array (BackendTask error a)
        arr =
            Array.fromList inputs

        go : Int -> Int -> BackendTask error (Rope a)
        go fromIncluded toExcluded =
            let
                sliceSize : Int
                sliceSize =
                    toExcluded - fromIncluded
            in
            if sliceSize > 128 then
                let
                    mid : Int
                    mid =
                        fromIncluded + sliceSize // 2
                in
                go fromIncluded mid
                    |> BackendTask.andThen
                        (\b ->
                            BackendTask.map (Rope.appendTo b)
                                (go mid toExcluded)
                        )

            else
                arr
                    |> Array.slice fromIncluded toExcluded
                    |> Array.toList
                    |> BackendTask.sequence
                    |> BackendTask.map Rope.fromList
    in
    go 0 (Array.length arr)
        |> BackendTask.map Rope.toList


sequence_ : List (BackendTask error ()) -> BackendTask error ()
sequence_ inputs =
    let
        arr : Array (BackendTask error ())
        arr =
            Array.fromList inputs

        go : Int -> Int -> BackendTask error ()
        go fromIncluded toExcluded =
            let
                sliceSize : Int
                sliceSize =
                    toExcluded - fromIncluded
            in
            if sliceSize > 128 then
                let
                    mid : Int
                    mid =
                        fromIncluded + sliceSize // 2
                in
                go fromIncluded mid
                    |> BackendTask.andThen
                        (\_ ->
                            go mid toExcluded
                        )

            else
                arr
                    |> Array.slice fromIncluded toExcluded
                    |> Array.toList
                    |> BackendTask.sequence
                    |> BackendTask.map (\_ -> ())
    in
    go 0 (Array.length arr)


profiling : String -> BackendTask FatalError a -> BackendTask FatalError a
profiling label t =
    Do.do (BackendTask.Customs.profile label) <| \_ ->
    Do.do t <| \res ->
    Do.do (BackendTask.Customs.profileEnd label) <| \_ ->
    BackendTask.succeed res


finally : BackendTask e () -> BackendTask e a -> BackendTask e a
finally afterTask task =
    task
        |> BackendTask.onError
            (\e ->
                afterTask |> BackendTask.andThen (\_ -> BackendTask.fail e)
            )
        |> BackendTask.andThen
            (\r ->
                afterTask
                    |> BackendTask.map (\_ -> r)
            )
