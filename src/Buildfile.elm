module Buildfile exposing (build)

import ConcurrentTask exposing (ConcurrentTask)
import Elm
import Rule exposing (Rule)


build : ConcurrentTask e (List Rule)
build =
    [ Rule.getFiles "assets"
        |> Rule.andThen
            (\images ->
                images
                    |> List.filter isImage
                    |> Rule.combineMap
                        (\image ->
                            Rule.map (Tuple.pair image) <|
                                Rule.getSize image
                        )
            )
        |> Rule.do
            (Rule.writeCodegenFile [ "Images" ] <|
                \imagesWithSizes ->
                    let
                        _ =
                            Debug.log "Creating file" ()
                    in
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
    , Rule.getSize "assets/empty"
        |> Rule.do
            (Rule.writeFile "assets/empty" <|
                \_ ->
                    Debug.todo "Should not be called"
            )
    ]
        |> ConcurrentTask.batch


isImage : String -> Bool
isImage path =
    String.endsWith ".png" path
        || String.endsWith ".webp" path
        || String.endsWith ".jpeg" path
        || String.endsWith ".jpg" path
        || String.endsWith ".git" path
        || String.endsWith ".tif" path
        || String.endsWith ".tiff" path
