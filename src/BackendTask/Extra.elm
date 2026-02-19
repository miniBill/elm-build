module BackendTask.Extra exposing (combine, combineBy, combineBy_, profiling, timed)

import Array exposing (Array)
import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.Time
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import List.Extra
import Pages.Script.Spinner as Spinner
import Rope exposing (Rope)
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


combine : List (BackendTask error a) -> BackendTask error (List a)
combine inputs =
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
                BackendTask.map2 Rope.appendTo
                    (go fromIncluded mid)
                    (go mid toExcluded)

            else
                arr
                    |> Array.slice fromIncluded toExcluded
                    |> Array.toList
                    |> BackendTask.combine
                    |> BackendTask.map Rope.fromList
    in
    go 0 (Array.length arr)
        |> BackendTask.map Rope.toList


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
                            BackendTask.map (Rope.prependTo b)
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
