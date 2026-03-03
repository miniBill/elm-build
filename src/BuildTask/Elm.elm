module BuildTask.Elm exposing (codegen, format)

import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Do as Do
import BuildTask.Unsafe as Unsafe
import Elm
import Path exposing (Path)


codegen : Elm.File -> BuildTask { filename : Path, hash : FileOrDirectory }
codegen file =
    Do.writeFile file.contents <| \hash ->
    BuildTask.do (format hash) <| \formatted ->
    BuildTask.succeed { filename = Path.path ("generated/" ++ file.path), hash = formatted }


format : FileOrDirectory -> BuildTask FileOrDirectory
format hash =
    Unsafe.pipeThrough "elm-format" [ "--stdin" ] hash
