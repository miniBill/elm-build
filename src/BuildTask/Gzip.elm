module BuildTask.Gzip exposing (Gzip, gunzip, gzip, which)

import BackendTask.Stream as Stream
import BuildTask exposing (Command, FileOrDirectory)
import BuildTask.Internal exposing (BuildTask)
import BuildTask.Unsafe
import FatalError exposing (FatalError)


type Gzip
    = Pigz Command


which : BuildTask FatalError Gzip
which =
    BuildTask.which "pigz" |> BuildTask.map Pigz


gzip :
    { tools | gzip : Gzip }
    -> FileOrDirectory
    ->
        BuildTask
            { fatal : FatalError
            , recoverable : Stream.Error () String
            }
            FileOrDirectory
gzip tools =
    let
        (Pigz pigz) =
            tools.gzip
    in
    BuildTask.Unsafe.pipeThrough pigz [ "-9" ]


gunzip :
    { tools | gzip : Gzip }
    -> FileOrDirectory
    ->
        BuildTask
            { fatal : FatalError
            , recoverable : Stream.Error () String
            }
            FileOrDirectory
gunzip tools =
    let
        (Pigz pigz) =
            tools.gzip
    in
    BuildTask.Unsafe.pipeThrough pigz [ "-d" ]
