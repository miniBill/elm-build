module BuildTask.Elm exposing (codegen, format)

import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Do as Do
import BuildTask.Unsafe as Unsafe
import Hash
import Path exposing (Path)


codegen :
    { path : String
    , contents : String
    , warnings :
        List
            { declaration : String
            , warning : String
            }
    }
    -> BuildTask { filename : Path, hash : FileOrDirectory }
codegen file =
    if String.isEmpty (String.trim file.contents) then
        if List.isEmpty file.warnings then
            BuildTask.fail ("Will not write an empty file at " ++ file.path)

        else
            BuildTask.fail ("Will not write an empty file at " ++ file.path ++ "\nWarnings were:" ++ String.concat (List.map (\w -> "- " ++ formatWarning w) file.warnings))

    else
        Do.writeFile file.contents <| \hash ->
        BuildTask.do (format hash) <| \formatted ->
        Do.withFile formatted
            (\content ->
                if String.isEmpty (String.trim content) then
                    BuildTask.fail ("elm-format produced an empty file - input was " ++ Hash.toString hash)

                else
                    BuildTask.succeed ()
            )
        <| \() ->
        { filename = Path.path ("generated/" ++ file.path)
        , hash = formatted
        }
            |> BuildTask.succeed
            |> BuildTask.withWarnings (List.map formatWarning file.warnings)


formatWarning : { declaration : String, warning : String } -> BuildTask.Warning
formatWarning warning =
    "In declaration " ++ warning.declaration ++ ": " ++ warning.warning


format : FileOrDirectory -> BuildTask FileOrDirectory
format hash =
    Unsafe.pipeThrough "elm-format" [ "--stdin" ] hash
