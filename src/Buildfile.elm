module Buildfile exposing (build)

import Rule exposing (RuleGenerator)
import String.Multiline


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
                            String.Multiline.here
                                """
                                    module Images exposing (a)
                                    a=0
                                    {-
                                    """
                                ++ String.join "\n"
                                    (List.map
                                        (\( image, size ) ->
                                            image ++ ": " ++ Maybe.withDefault "?" (Maybe.map String.fromInt size) ++ " bytes"
                                        )
                                        imagesWithSizes
                                    )
                                ++ String.Multiline.here """
                                    -}
                                    """
                        )
                    |> Rule.writeFile "generated/Images.elm"
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
