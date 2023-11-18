module Buildfile exposing (build)

import ConcurrentTask exposing (ConcurrentTask)
import Elm
import Rule exposing (Rule, TrackingTask)


build : ConcurrentTask e (List Rule)
build =
    [ images
        |> (Rule.writeCodegenFile [ "Images" ] <|
                \imagesWithSizes ->
                    imagesWithSizes
                        |> List.map
                            (\path ->
                                let
                                    name =
                                        String.dropLeft (String.length "assets/") path
                                in
                                Elm.declaration name <|
                                    Elm.string path
                            )
           )
    ]
        |> ConcurrentTask.batch


images : TrackingTask (List String)
images =
    Rule.listFiles "assets"
        |> Rule.map (List.filter isImage)


isImage : String -> Bool
isImage path =
    String.endsWith ".png" path
        || String.endsWith ".webp" path
        || String.endsWith ".jpeg" path
        || String.endsWith ".jpg" path
        || String.endsWith ".git" path
        || String.endsWith ".tif" path
        || String.endsWith ".tiff" path
