module Buildfile exposing (build)

import Elm
import Rule exposing (RuleGenerator)


build : List RuleGenerator
build =
    [ Rule.getFiles "assets"
        |> Rule.map (List.filter isImage)
        |> Rule.do
            (\images ->
                images
                    |> Rule.combineMap
                        (\image ->
                            Rule.map (Tuple.pair image) <|
                                Rule.getSize image
                        )
                    |> Rule.map
                        (\imagesWithSizes ->
                            imagesWithSizes
                                |> List.map
                                    (\( image, size ) ->
                                        Elm.declaration image <|
                                            Elm.string <|
                                                image
                                                    ++ ": "
                                                    ++ Maybe.withDefault "?"
                                                        (Maybe.map String.fromInt size)
                                    )
                        )
                    |> Rule.writeCodegenFile [ "Images" ]
            )
    ]


isImage : String -> Bool
isImage path =
    String.endsWith ".png" path
        || String.endsWith ".webp" path
        || String.endsWith ".jpeg" path
        || String.endsWith ".jpg" path
        || String.endsWith ".git" path
        || String.endsWith ".tif" path
        || String.endsWith ".tiff" path
