module BuildTask.Elm exposing (codegen, format)

import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Do as Do
import BuildTask.Unsafe as Unsafe
import BuildTask.Unsafe.Do as Do
import Elm
import Path exposing (Path)


codegen : Elm.File -> BuildTask { filename : Path, hash : FileOrDirectory }
codegen file =
    Do.writeFile file.contents <| \hash ->
    Do.pipeThrough "elm-format" [ "--stdin" ] hash <| \formatted ->
    BuildTask.succeed { filename = Path.path ("generated/" ++ file.path), hash = formatted }


format : FileOrDirectory -> BuildTask FileOrDirectory
format hash =
    Unsafe.pipeThrough "elm-format" [ "--stdin" ] hash
