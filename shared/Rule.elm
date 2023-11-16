module Rule exposing (Path, Rule, RuleGenerator, TrackingTask, getFiles, getInputs, getMTime, getRule, getRules, map, writeFile)

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
    TrackingTask ( List Path, ConcurrentTask Never () )


type TrackingTask a
    = TrackingTask (List Path) (ConcurrentTask Never a)


map : (a -> b) -> TrackingTask a -> TrackingTask b
map f (TrackingTask inputs task) =
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
            (\( outputs, task ) ->
                { inputs = inputs
                , outputs = outputs
                , task = task
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


writeFile : Path -> String -> ( List Path, ConcurrentTask e () )
writeFile path content =
    ( [ path ]
    , { function = "writeFile"
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


getMTime : Path -> ConcurrentTask e (Maybe Time.Posix)
getMTime path =
    { function = "getMTime"
    , expect =
        ConcurrentTask.expectJson
            (JD.nullable <| JD.map (Time.millisToPosix << round) JD.float)
    , errors = ConcurrentTask.expectNoErrors
    , args = JE.string path
    }
        |> ConcurrentTask.define
