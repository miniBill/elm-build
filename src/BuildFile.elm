module BuildFile exposing (BuildFile)

import BackendTask exposing (BackendTask)
import BuildTask exposing (BuildTask, FileOrDirectory)
import FatalError exposing (FatalError)
import Path exposing (Path)


{-| -}
type alias BuildFile tools inputs =
    { getTools : inputs -> BuildTask () FatalError tools
    , getInputs :
        { inputPath : Path, buildPath : Path, debug : Bool }
        -> BackendTask FatalError inputs
    , buildAction :
        { inputPath : Path, buildPath : Path }
        -> inputs
        -> BuildTask tools FatalError FileOrDirectory
    }
