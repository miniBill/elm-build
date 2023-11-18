module Buildfile exposing (build)

import Elm
import Rule exposing (Rules)
import TrackingTask exposing (TrackingTask)


build : List Rules
build =
    [ Rule.writeCodegenFile images [ "Images" ] <|
        List.map
            (\path ->
                let
                    name : String
                    name =
                        getFileName path
                in
                Elm.declaration name <|
                    Elm.string ("images/" ++ name ++ ".webp")
            )
    , Rule.list images <|
        \path ->
            Rule.convert path ("images/" ++ getFileName path ++ ".webp")
    ]


getFileName : String -> String
getFileName path =
    let
        filename : String
        filename =
            path
                |> String.split "/"
                |> List.reverse
                |> List.head
                |> Maybe.withDefault path
    in
    filename
        |> String.split "."
        |> List.head
        |> Maybe.withDefault filename


images : TrackingTask (List String)
images =
    Rule.listFiles "assets"
        |> TrackingTask.map (List.filter isImage)


isImage : String -> Bool
isImage path =
    String.endsWith ".png" path
        || String.endsWith ".webp" path
        || String.endsWith ".jpeg" path
        || String.endsWith ".jpg" path
        || String.endsWith ".git" path
        || String.endsWith ".tif" path
        || String.endsWith ".tiff" path
