module BackendTask.Customs exposing (profile, profileEnd, triggerDebugger)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode


profile : String -> BackendTask FatalError ()
profile label =
    BackendTask.Custom.run "profile" (Json.Encode.string label) (Json.Decode.succeed ())
        |> BackendTask.allowFatal


profileEnd : String -> BackendTask FatalError ()
profileEnd label =
    BackendTask.Custom.run "profileEnd" (Json.Encode.string label) (Json.Decode.succeed ())
        |> BackendTask.allowFatal


triggerDebugger : BackendTask FatalError ()
triggerDebugger =
    BackendTask.Custom.run "triggerDebugger" Json.Encode.null (Json.Decode.succeed ())
        |> BackendTask.allowFatal
