module Internal exposing (Path, TrackingTask(..))

import ConcurrentTask exposing (ConcurrentTask)


type alias Path =
    String


type TrackingTask a
    = TrackingTask (ConcurrentTask Never ( List Path, a ))
