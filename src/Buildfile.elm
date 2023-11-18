module Buildfile exposing (build)

import ConcurrentTask exposing (ConcurrentTask)
import Elm
import Rule exposing (Rule, TrackingTask)


build : ConcurrentTask e (List Rule)
build =
    [ Rule.writeCodegenFile images [ "Images" ] <|
        \paths ->
            paths
                |> List.map
                    (\path ->
                        let
                            filename : String
                            filename =
                                path
                                    |> String.split "/"
                                    |> List.reverse
                                    |> List.head
                                    |> Maybe.withDefault path

                            name : String
                            name =
                                filename
                                    |> String.split "."
                                    |> List.head
                                    |> Maybe.withDefault filename
                        in
                        Elm.declaration name <|
                            Elm.string ("images/" ++ name ++ ".webp")
                    )
    , Rule.multiple images <|
        \paths ->
            List.map
                (\path ->
                    let
                        filename : String
                        filename =
                            path
                                |> String.split "/"
                                |> List.reverse
                                |> List.head
                                |> Maybe.withDefault path

                        name : String
                        name =
                            filename
                                |> String.split "."
                                |> List.head
                                |> Maybe.withDefault filename
                    in
                    Rule.convert path ("images/" ++ name ++ ".webp")
                )
                paths
    ]
        |> ConcurrentTask.batch
        |> ConcurrentTask.map List.concat


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
