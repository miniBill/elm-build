module BuildTask.Tar exposing (extract, listContents)

import BackendTask.Do
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Internal as Internal
import BuildTask.Unsafe
import Hash
import Pages.Script as Script


{-| List the contents of a tar file
-}
listContents : FileOrDirectory -> BuildTask (List String)
listContents tar =
    BuildTask.do (BuildTask.Unsafe.pipeThrough "tar" [ "tf", "-" ] tar) <| \contentsFile ->
    BuildTask.withFile contentsFile (\raw -> BuildTask.succeed (String.split "\n" raw))


{-| Extract files, optionally stripping a prefix. You don't need to specify the prefix in the list of files to extract.
-}
extract : { stripPrefix : Maybe String } -> FileOrDirectory -> List String -> BuildTask FileOrDirectory
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
    Internal.derive "tar xf" outputHash <| \{ prefix, buildPath } target ->
    BackendTask.Do.do (Script.makeDirectory { recursive = True } (Hash.toPathTemporary buildPath target)) <| \() ->
    Internal.execLog prefix
        "tar"
        ([ "xf"
         , Hash.toPath buildPath input
         , "-C"
         , Hash.toPathTemporary buildPath target
         ]
            ++ filesArgs
        )
