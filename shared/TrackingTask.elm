module TrackingTask exposing (TrackingTask, andThen, batch, combineMap, map, map2, succeed, toConcurrentTask)

import ConcurrentTask exposing (ConcurrentTask)
import Internal


type alias TrackingTask a =
    Internal.TrackingTask a


succeed : a -> TrackingTask a
succeed value =
    Internal.TrackingTask (ConcurrentTask.succeed ( [], value ))


map : (a -> b) -> TrackingTask a -> TrackingTask b
map f (Internal.TrackingTask task) =
    Internal.TrackingTask (ConcurrentTask.map (\( inputs, x ) -> ( inputs, f x )) task)


map2 : (a -> b -> c) -> TrackingTask a -> TrackingTask b -> TrackingTask c
map2 f (Internal.TrackingTask ltask) (Internal.TrackingTask rtask) =
    Internal.TrackingTask
        (ConcurrentTask.map2
            (\( linputs, lx ) ( rinputs, rx ) ->
                ( linputs ++ rinputs, f lx rx )
            )
            ltask
            rtask
        )


andThen : (a -> TrackingTask b) -> TrackingTask a -> TrackingTask b
andThen f (Internal.TrackingTask task) =
    task
        |> ConcurrentTask.andThen
            (\( xi, x ) ->
                let
                    (Internal.TrackingTask fx) =
                        f x
                in
                ConcurrentTask.map
                    (\( fxi, v ) ->
                        ( fxi ++ xi
                        , v
                        )
                    )
                    fx
            )
        |> Internal.TrackingTask


batch : List (TrackingTask a) -> TrackingTask (List a)
batch tasks =
    List.foldl
        (\item acc -> map2 (::) item acc)
        (succeed [])
        tasks


combineMap : (a -> TrackingTask b) -> List a -> TrackingTask (List b)
combineMap f items =
    List.foldl
        (\item acc -> map2 (::) (f item) acc)
        (succeed [])
        items


toConcurrentTask : TrackingTask a -> ConcurrentTask e a
toConcurrentTask (Internal.TrackingTask task) =
    task
        |> ConcurrentTask.mapError never
        |> ConcurrentTask.map Tuple.second
