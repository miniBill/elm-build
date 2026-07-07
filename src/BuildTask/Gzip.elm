module BuildTask.Gzip exposing (gunzip, gzip)

import BackendTask.Stream as Stream
import BuildTask exposing (FileOrDirectory)
import BuildTask.Internal exposing (BuildTask, Error)
import BuildTask.Unsafe
import FatalError exposing (FatalError)


gzip :
    FileOrDirectory
    ->
        BuildTask
            { fatal : FatalError
            , recoverable : Stream.Error () String
            }
            FileOrDirectory
gzip =
    BuildTask.Unsafe.pipeThrough "pigz" [ "-9" ]


gunzip :
    FileOrDirectory
    ->
        BuildTask
            { fatal : FatalError
            , recoverable : Stream.Error () String
            }
            FileOrDirectory
gunzip =
    BuildTask.Unsafe.pipeThrough "gunzip" []
