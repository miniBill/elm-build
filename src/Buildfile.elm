module Buildfile exposing (build)

import Rule exposing (RuleGenerator)
import String.Multiline


build : List RuleGenerator
build =
    [ Rule.getFiles "assets"
        |> Rule.map (List.filter isImage)
        |> Rule.map
            (\images ->
                Rule.writeFile "generated/Images.elm" <|
                    String.Multiline.here <|
                        """
                        module Images exposing (..)
                        a=0
                        {- """
                            ++ String.join "\n" images
                            ++ """
                        -}
                        """
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
