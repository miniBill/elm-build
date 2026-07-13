module BuildFile exposing (BuildFile)

import BackendTask exposing (BackendTask)
import BackendTask.Glob as Glob
import BuildTask exposing (BuildTask, Command, FileOrDirectory)
import BuildTask.Do as Do
import BuildTask.Elm as Elm
import BuildTask.Font as Font
import BuildTask.Image as Image
import BuildTask.Unsafe as Unsafe
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Declare
import Elm.Let
import Elm.Op
import Elm.Op.Extra
import FatalError exposing (FatalError)
import Gen.Html
import Gen.Html.Attributes
import Gen.Html.Picture
import Gen.Html.Source
import Gen.List
import Gen.String
import List.Extra
import Maybe.Extra
import Pages.Script as Script
import Path exposing (Path)
import String.Extra
import Utils


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
