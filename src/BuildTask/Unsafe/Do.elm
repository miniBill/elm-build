module BuildTask.Unsafe.Do exposing (commandWithFile, pipeThrough)

import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Do exposing (andThen)
import BuildTask.Unsafe


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
    String
    -> List String
    -> FileOrDirectory
    -> (FileOrDirectory -> BuildTask a)
    -> BuildTask a
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
    String
    -> List String
    -> FileOrDirectory
    -> (FileOrDirectory -> BuildTask a)
    -> BuildTask a
commandWithFile cmd args hash k =
    BuildTask.Unsafe.commandWithFile cmd args hash |> andThen k
