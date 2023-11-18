module Rule exposing (Path, Rule, TrackingTask, andThen, batch, combineMap, getFiles, getMTime, getSize, map, map2, toConcurrentTask, writeCodegenFile, writeFile)

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
    , task : () -> ConcurrentTask Never ()
    }


do : List Path -> (a -> ConcurrentTask Never ()) -> TrackingTask a -> ConcurrentTask e Rule
do outputs f (TrackingTask task) =
    ConcurrentTask.map
        (\( inputs, a ) ->
            { inputs = inputs
            , outputs = outputs
            , task = \_ -> f a
            }
        )
        (ConcurrentTask.mapError never task)


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


writeFile :
    Path
    -> (a -> String)
    -> TrackingTask a
    -> ConcurrentTask e Rule
writeFile path content =
    do
        [ path ]
        (\a ->
            { function = "writeFile"
            , expect = ConcurrentTask.expectWhatever
            , errors = ConcurrentTask.expectNoErrors
            , args =
                JE.object
                    [ ( "path", JE.string path )
                    , ( "content", JE.string (content a) )
                    ]
            }
                |> ConcurrentTask.define
        )


writeCodegenFile :
    List String
    -> (a -> List Elm.Declaration)
    -> TrackingTask a
    -> ConcurrentTask e Rule
writeCodegenFile moduleName declarations task =
    writeFile
        (String.join "/" ("generated" :: moduleName) ++ ".elm")
        (\a -> (Elm.file moduleName (declarations a)).contents)
        task


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
