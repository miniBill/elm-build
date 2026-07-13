module Example exposing (run)

import Build
import BuildFile.Example
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withCliOptions
        (Build.programConfig BuildFile.Example.buildFile)
        Build.toTask
