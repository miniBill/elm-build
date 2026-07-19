module BuildTask.Unsafe.Do exposing (commandWithFile, downloadImmutable, pipeThrough)

import BackendTask.Stream as Stream
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Do exposing (andThen)
import BuildTask.Unsafe
import FatalError exposing (FatalError)


{-| Pipe a file through a command.

**CORRECTNESS:**
The command must be deterministic. The same input should correspond to the same output.

In particular, the command must not:

  - get data from the internet,
  - read other files,
  - use the current time,
  - use any other source of randomness.

-}
pipeThrough :
    BuildTask.Command
    -> List String
    -> FileOrDirectory
    -> (FileOrDirectory -> BuildTask { fatal : FatalError, recoverable : Stream.Error () String } a)
    -> BuildTask { fatal : FatalError, recoverable : Stream.Error () String } a
pipeThrough cmd args hash k =
    BuildTask.Unsafe.pipeThrough cmd args hash |> andThen k


{-| Run a command passing in a file (or directory) as last argument and save the result to a file.

**CORRECTNESS:**
The command must be deterministic. The same input should correspond to the same output.

In particular, the command must not:

  - get data from the internet,
  - read other files,
  - use the current time,
  - use any other source of randomness.

-}
commandWithFile :
    BuildTask.Command
    -> List String
    -> FileOrDirectory
    -> (FileOrDirectory -> BuildTask { fatal : FatalError, recoverable : Stream.Error Int String } a)
    -> BuildTask { fatal : FatalError, recoverable : Stream.Error Int String } a
commandWithFile cmd args hash k =
    BuildTask.Unsafe.commandWithFile cmd args hash |> andThen k


{-| Downloads a file given its URL.

**CORRECTNESS:**
The file must never change on the server.

-}
downloadImmutable :
    String
    -> (FileOrDirectory -> BuildTask { recoverable : Stream.Error () String, fatal : FatalError } a)
    -> BuildTask { recoverable : Stream.Error () String, fatal : FatalError } a
downloadImmutable url k =
    BuildTask.Unsafe.downloadImmutable url |> andThen k
