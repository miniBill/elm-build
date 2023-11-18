module Internal exposing (Path, RuleData, Rules(..), TrackingTask(..))

import ConcurrentTask exposing (ConcurrentTask)
import Set exposing (Set)


type alias Path =
    String


type Rules
    = Rules (ConcurrentTask Never (List RuleData))


type alias RuleData =
    { inputs : Set Path
    , outputs : Set Path
    , task : () -> ConcurrentTask String ()
    , taskDescription : List String
    }


type TrackingTask a
    = TrackingTask (ConcurrentTask Never ( List Path, a ))
