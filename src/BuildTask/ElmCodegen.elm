module BuildTask.ElmCodegen exposing (elmCodegen)

import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Do as Do
import Elm
import Path exposing (Path)


elmCodegen : Elm.File -> BuildTask { filename : Path, hash : FileOrDirectory }
elmCodegen file =
    Do.writeFile file.contents <| \hash ->
    Do.pipeThrough "elm-format" [ "--stdin" ] hash <| \formatted ->
    BuildTask.succeed { filename = Path.path ("generated/" ++ file.path), hash = formatted }
