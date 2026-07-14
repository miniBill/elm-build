module BuildTask.Elm exposing (CodegenError, codegen, format)

import BackendTask.File as File
import BackendTask.Stream as Stream
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Unsafe as Unsafe
import FatalError exposing (FatalError)
import Hash
import Pages.Script as Script
import Path.Posix as Path exposing (Path)


type CodegenError
    = ErrorWritingUnformattedFile Script.Error
    | ErrorFormatting (Stream.Error () String)
    | ErrorReadingFormattedFile (File.FileReadError Never)
    | ElmFormatProducedAnEmptyFileFrom FileOrDirectory


codegen :
    { tools | elm_format : BuildTask.Command }
    ->
        { path : Path Path.Relative Path.File
        , contents : String
        , warnings :
            List
                { declaration : String
                , warning : String
                }
        }
    ->
        BuildTask
            { fatal : FatalError, recoverable : CodegenError }
            { filename : Path Path.Relative Path.File
            , hash : FileOrDirectory
            }
codegen tools file =
    BuildTask.doWithError (BuildTask.writeFile file.contents) ErrorWritingUnformattedFile <| \hash ->
    BuildTask.doWithError (format tools hash) ErrorFormatting <| \formatted ->
    BuildTask.doWithError (BuildTask.withFile formatted BuildTask.succeed) ErrorReadingFormattedFile <| \content ->
    BuildTask.do
        (if String.isEmpty (String.trim content) then
            FatalError.recoverable
                { title = "Empty elm-format output"
                , body = "elm-format produced an empty file - input was " ++ Hash.toString hash
                }
                (ElmFormatProducedAnEmptyFileFrom hash)
                |> BuildTask.fail

         else
            BuildTask.succeed ()
        )
    <| \() ->
    case Path.parseRelativeDirectory "generated" of
        Err _ ->
            Debug.todo "Should not happen"

        Ok generated ->
            { filename = Path.append generated file.path
            , hash = formatted
            }
                |> BuildTask.succeed
                |> BuildTask.withWarnings (List.map formatWarning file.warnings)


formatWarning : { declaration : String, warning : String } -> BuildTask.Warning
formatWarning warning =
    "In declaration " ++ warning.declaration ++ ": " ++ warning.warning


format : { tools | elm_format : BuildTask.Command } -> FileOrDirectory -> BuildTask { fatal : FatalError, recoverable : Stream.Error () String } FileOrDirectory
format { elm_format } hash =
    Unsafe.pipeThrough elm_format [ "--stdin" ] hash
