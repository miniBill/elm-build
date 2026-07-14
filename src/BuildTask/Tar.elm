module BuildTask.Tar exposing (extract, listContents)

import BackendTask
import BackendTask.Do
import BackendTask.File.Extra
import BuildTask exposing (BuildTask, Command, FileOrDirectory)
import BuildTask.Internal as Internal
import BuildTask.Unsafe
import FatalError exposing (FatalError)
import Hash
import Path.Posix as Path


{-| List the contents of a tar file
-}
listContents : FileOrDirectory -> BuildTask { tools | tar : Command } FatalError (List String)
listContents tarFile =
    BuildTask.do (BuildTask.Unsafe.pipeThrough .tar [ "tf", "-" ] tarFile |> BuildTask.allowFatal) <| \contentsFile ->
    BuildTask.withFile contentsFile (\raw -> BuildTask.succeed (String.split "\n" raw)) |> BuildTask.allowFatal


{-| Extract files, optionally stripping a prefix. You don't need to specify the prefix in the list of files to extract.
-}
extract :
    { stripPrefix : Maybe String }
    -> FileOrDirectory
    -> List String
    -> BuildTask { tools | tar : Command } FatalError FileOrDirectory
extract { stripPrefix } input files =
    let
        ( outputHashTask, filesArgs ) =
            case stripPrefix of
                Nothing ->
                    ( Internal.extendHashWith files input, files )

                Just prefix ->
                    ( Internal.extendHashWith ("--strip-components" :: prefix :: files) input
                    , "--strip-components" :: String.fromInt (1 + List.length (String.indexes "/" prefix)) :: List.map (\file -> prefix ++ "/" ++ file) files
                    )
    in
    BuildTask.do outputHashTask <| \outputHash ->
    BuildTask.do (BuildTask.getTool .tar) <| \tar ->
    Internal.deriveDirectory "tar xf" outputHash <| \({ buildPath } as input_) target ->
    BackendTask.Do.do
        (BackendTask.File.Extra.makeDirectory { recursive = True } target
            |> BackendTask.mapError Internal.InternalError
        )
    <| \() ->
    Internal.execLog input_
        tar.name
        ([ "xf"
         , Path.toString (Hash.toFilePath buildPath input)
         , "-C"
         , Path.toString target
         ]
            ++ filesArgs
        )
        |> BackendTask.mapError Internal.UserError
