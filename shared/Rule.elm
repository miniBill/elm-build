module Rule exposing (Path, Rule, RuleGenerator, TrackingTask, batch, combineMap, do, getFiles, getInputs, getMTime, getRule, getRules, getSize, getTask, map, map2, writeFile)

import ConcurrentTask exposing (ConcurrentTask)
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


type alias RuleGenerator =
    TrackingTask Rule


do : (a -> Rule) -> TrackingTask a -> RuleGenerator
do f (TrackingTask inputs task) =
    TrackingTask inputs (ConcurrentTask.map f task)


getInputs : TrackingTask a -> List Path
getInputs (TrackingTask inputs _) =
    inputs


getRules : List RuleGenerator -> ConcurrentTask e (List Rule)
getRules generators =
    ConcurrentTask.batch (List.map getRule generators)


getRule : RuleGenerator -> ConcurrentTask e Rule
getRule (TrackingTask inputs toRule) =
    toRule
        |> ConcurrentTask.mapError never
        |> ConcurrentTask.map
            (\rule ->
                { rule
                    | inputs = inputs ++ rule.inputs
                }
            )


getFiles : Path -> TrackingTask (List Path)
getFiles path =
    { function = "getFiles"
    , expect = ConcurrentTask.expectJson (JD.list JD.string)
    , errors = ConcurrentTask.expectNoErrors
    , args = JE.string path
    }
        |> ConcurrentTask.define
        |> TrackingTask [ path ]


writeFile :
    Path
    -> TrackingTask String
    ->
        { inputs : List Path
        , outputs : List Path
        , task : ConcurrentTask e ()
        }
writeFile path (TrackingTask inputs task) =
    { inputs = inputs
    , outputs = [ path ]
    , task =
        task
            |> ConcurrentTask.mapError never
            |> ConcurrentTask.andThen
                (\content ->
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
                )
    }


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
    TrackingTask [ path ]
        ({ function = "stat"
         , expect = ConcurrentTask.expectJson (JD.nullable statDecoder)
         , errors = ConcurrentTask.expectNoErrors
         , args = JE.string path
         }
            |> ConcurrentTask.define
        )


statDecoder : JD.Decoder Stat
statDecoder =
    JD.map2 Stat
        (JD.field "size" JD.int)
        (JD.map (Time.millisToPosix << round) <| JD.field "mtimeMs" JD.float)



-- TrackingTask --


type TrackingTask a
    = TrackingTask (List Path) (ConcurrentTask Never a)


succeed : a -> TrackingTask a
succeed value =
    TrackingTask [] (ConcurrentTask.succeed value)


map : (a -> b) -> TrackingTask a -> TrackingTask b
map f (TrackingTask inputs task) =
    TrackingTask inputs (ConcurrentTask.map f task)


map2 : (a -> b -> c) -> TrackingTask a -> TrackingTask b -> TrackingTask c
map2 f (TrackingTask linputs ltask) (TrackingTask rinputs rtask) =
    TrackingTask (linputs ++ rinputs) (ConcurrentTask.map2 f ltask rtask)


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


getTask : TrackingTask a -> ConcurrentTask e a
getTask (TrackingTask _ task) =
    ConcurrentTask.mapError never task
