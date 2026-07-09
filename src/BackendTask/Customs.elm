module BackendTask.Customs exposing (profile, profileEnd, readdir, triggerDebugger, writeBinaryFile)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import Bytes exposing (Bytes)
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import Path exposing (Path)
import XBytes


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


writeBinaryFile : { path : String, body : Bytes } -> BackendTask FatalError ()
writeBinaryFile { path, body } =
    BackendTask.Custom.run "writeBinaryFile"
        (Json.Encode.object
            [ ( "path", Json.Encode.string path )
            , ( "body", Json.Encode.string (XBytes.toHex (XBytes.fromBytes body)) )
            ]
        )
        (Json.Decode.succeed ())
        |> BackendTask.allowFatal
