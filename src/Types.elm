module Types exposing (Block(..), Command, Engine(..))

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)


type Engine
    = Engine (BackendTask FatalError (List Block))


type Block
    = Pool { name : String, depth : Int }
    | Rule { name : String, commands : List Command, pool : Maybe String }
    | Build { outputs : List String, rule : String, inputs : List String }


type alias Command =
    List String
