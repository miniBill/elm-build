module Rule exposing (Path, Rule, TrackingTask, andThen, batch, combineMap, do, getFiles, getMTime, getSize, map, map2, toConcurrentTask, writeCodegenFile, writeFile)

import ConcurrentTask exposing (ConcurrentTask)
import Elm
import Json.Decode as JD
import Json.Encode as JE
import Time


type alias Path =
    String


type alias Rule =
    { inputs : List Path
    , outputs : List Path
    , task : ConcurrentTask Never ()
    }


type alias Action =
    { outputs : List Path
    , task : ConcurrentTask Never ()
    }


do : (a -> Action) -> TrackingTask a -> ConcurrentTask e Rule
do f (TrackingTask task) =
    ConcurrentTask.map
        (\( inputs, a ) ->
            let
                fa : Action
                fa =
                    f a
            in
            { inputs = inputs
            , outputs = fa.outputs
            , task = fa.task
            }
        )
        (ConcurrentTask.onError never task)


getFiles : Path -> TrackingTask (List Path)
getFiles path =
    { function = "getFiles"
    , expect = ConcurrentTask.expectJson (JD.list JD.string)
    , errors = ConcurrentTask.expectNoErrors
    , args = JE.string path
    }
        |> ConcurrentTask.define
        |> ConcurrentTask.map (\v -> ( [ path ], v ))
        |> TrackingTask


writeFile : Path -> String -> Action
writeFile path content =
    { outputs = [ path ]
    , task =
        { function = "writeFile"
        , expect = ConcurrentTask.expectWhatever
        , errors = ConcurrentTask.expectNoErrors
        , args =
            JE.object
                [ ( "path", JE.string path )
                , ( "content", JE.string content )
                ]
        }
            |> ConcurrentTask.define
    }


writeCodegenFile : Elm.File -> Action
writeCodegenFile file =
    writeFile
        ("generated/" ++ file.path)
        file.contents


getMTime : Path -> TrackingTask (Maybe Time.Posix)
getMTime path =
    stat path |> map (Maybe.map <| \{ mtime } -> mtime)


getSize : Path -> TrackingTask (Maybe Int)
getSize path =
    stat path |> map (Maybe.map <| \{ size } -> size)


type alias Stat =
    { size : Int
    , mtime : Time.Posix
    }


stat : String -> TrackingTask (Maybe Stat)
stat path =
    TrackingTask
        ({ function = "stat"
         , expect = ConcurrentTask.expectJson (JD.nullable statDecoder)
         , errors = ConcurrentTask.expectNoErrors
         , args = JE.string path
         }
            |> ConcurrentTask.define
            |> ConcurrentTask.map (\res -> ( [ path ], res ))
        )


statDecoder : JD.Decoder Stat
statDecoder =
    JD.map2 Stat
        (JD.field "size" JD.int)
        (JD.map (Time.millisToPosix << round) <| JD.field "mtimeMs" JD.float)



-- TrackingTask --


type TrackingTask a
    = TrackingTask (ConcurrentTask Never ( List Path, a ))


succeed : a -> TrackingTask a
succeed value =
    TrackingTask (ConcurrentTask.succeed ( [], value ))


map : (a -> b) -> TrackingTask a -> TrackingTask b
map f (TrackingTask task) =
    TrackingTask (ConcurrentTask.map (\( inputs, x ) -> ( inputs, f x )) task)


map2 : (a -> b -> c) -> TrackingTask a -> TrackingTask b -> TrackingTask c
map2 f (TrackingTask ltask) (TrackingTask rtask) =
    TrackingTask
        (ConcurrentTask.map2
            (\( linputs, lx ) ( rinputs, rx ) ->
                ( linputs ++ rinputs, f lx rx )
            )
            ltask
            rtask
        )


andThen : (a -> TrackingTask b) -> TrackingTask a -> TrackingTask b
andThen f (TrackingTask task) =
    task
        |> ConcurrentTask.andThen
            (\( xi, x ) ->
                let
                    (TrackingTask fx) =
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
        |> TrackingTask


batch : List (TrackingTask a) -> TrackingTask (List a)
batch list =
    List.foldl
        (\item acc -> map2 (::) item acc)
        (succeed [])
        list


combineMap : (a -> TrackingTask b) -> List a -> TrackingTask (List b)
combineMap f list =
    List.foldl
        (\item acc -> map2 (::) (f item) acc)
        (succeed [])
        list


toConcurrentTask : TrackingTask a -> ConcurrentTask e a
toConcurrentTask (TrackingTask task) =
    task
        |> ConcurrentTask.mapError never
        |> ConcurrentTask.map Tuple.second
