module BuildTask.Gzip exposing (gunzip, gzip)

import BackendTask.Stream as Stream
import BuildTask exposing (Command, FileOrDirectory)
import BuildTask.Internal exposing (BuildTask)
import BuildTask.Unsafe
import FatalError exposing (FatalError)


gzip :
    { tools | gzip : BuildTask.Command }
    -> FileOrDirectory
    ->
        BuildTask
            { fatal : FatalError
            , recoverable : Stream.Error () String
            }
            FileOrDirectory
gzip tools =
    BuildTask.Unsafe.pipeThrough tools.gzip [ "-9" ]


gunzip :
    { tools | gzip : BuildTask.Command }
    -> FileOrDirectory
    ->
        BuildTask
            { fatal : FatalError
            , recoverable : Stream.Error () String
            }
            FileOrDirectory
gunzip tools =
    BuildTask.Unsafe.pipeThrough tools.gzip [ "-d" ]
