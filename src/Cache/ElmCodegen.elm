module Cache.ElmCodegen exposing (elmCodegen)

import Cache exposing (FileOrDirectory)
import Cache.Do as Do
import Elm
import Path exposing (Path)


elmCodegen : Elm.File -> Cache.Monad { filename : Path, hash : FileOrDirectory }
elmCodegen file =
    Do.writeFile file.contents <| \hash ->
    Do.pipeThrough "elm-format" [ "--stdin" ] hash <| \formatted ->
    Cache.succeed { filename = Path.path ("generated/" ++ file.path), hash = formatted }
