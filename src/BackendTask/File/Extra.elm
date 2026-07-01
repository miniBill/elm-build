module BackendTask.File.Extra exposing (removeFileIfExists)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import FatalError exposing (FatalError)
import Pages.Script as Script


removeFileIfExists : String -> BackendTask FatalError ()
removeFileIfExists filename =
    Do.do (File.exists filename) <| \exists ->
    if exists then
        Do.exec "chmod" [ "-R", "700", filename ] <| \_ ->
        Script.exec "rm" [ "-r", filename ]

    else
        Do.noop
