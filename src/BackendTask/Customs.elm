module BackendTask.Customs exposing (profile, profileEnd, readdir, triggerDebugger)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import Path exposing (Path)


profile : String -> BackendTask FatalError ()
profile label =
    BackendTask.Custom.run "profile" (Json.Encode.string label) (Json.Decode.succeed ())
        |> BackendTask.allowFatal


profileEnd : String -> BackendTask FatalError ()
profileEnd label =
    BackendTask.Custom.run "profileEnd" (Json.Encode.string label) (Json.Decode.succeed ())
        |> BackendTask.allowFatal


readdir : Path -> BackendTask FatalError (List String)
readdir path =
    BackendTask.Custom.run "readdir"
        (Json.Encode.string (Path.toString path))
        (Json.Decode.list Json.Decode.string)
        |> BackendTask.allowFatal


triggerDebugger : BackendTask FatalError ()
triggerDebugger =
    BackendTask.Custom.run "triggerDebugger" Json.Encode.null (Json.Decode.succeed ())
        |> BackendTask.allowFatal
