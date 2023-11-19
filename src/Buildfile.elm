module Buildfile exposing (build)

import Bytes exposing (Bytes)
import Elm
import Elm.Annotation
import Image
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
                Elm.expose <|
                    Elm.declaration name <|
                        Elm.string ("images/" ++ name ++ ".webp")
            )
    , Rule.list images <|
        \path ->
            Rule.convert path ("images/" ++ getFileName path ++ ".webp")
    , Rule.writeCodegenFile gradients [ "Gradients" ] <|
        List.reverse
            << List.map gradientToDeclaration
    ]


gradientToDeclaration : ( String, Bytes ) -> Elm.Declaration
gradientToDeclaration ( name, gradient ) =
    let
        content : Elm.Expression
        content =
            case Image.decode gradient of
                Nothing ->
                    debugTodo <| Elm.string <| "Could not decode gradient: " ++ name

                Just decoded ->
                    Image.toList decoded
                        |> List.map
                            (\pixel ->
                                let
                                    r : Int
                                    r =
                                        modBy 256 <| pixel // (256 ^ 3)

                                    g : Int
                                    g =
                                        modBy 256 <| pixel // (256 ^ 2)

                                    b : Int
                                    b =
                                        modBy 256 <| pixel // (256 ^ 1)
                                in
                                Elm.triple (Elm.int r) (Elm.int g) (Elm.int b)
                            )
                        |> Elm.list
    in
    content
        |> Elm.declaration (String.replace "_g" "G" name)
        |> Elm.expose


debugTodo : Elm.Expression -> Elm.Expression
debugTodo arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Debug" ]
            , name = "todo"
            , annotation =
                Just <|
                    Elm.Annotation.function
                        [ Elm.Annotation.string ]
                        (Elm.Annotation.var "a")
            }
        )
        [ arg ]


gradients : TrackingTask (List ( String, Bytes ))
gradients =
    Rule.listFiles "assets"
        |> TrackingTask.map (List.filter (String.endsWith "_gradient.png"))
        |> TrackingTask.andThen
            (TrackingTask.combineMap
                (\path ->
                    TrackingTask.map
                        (Tuple.pair (getFileName path))
                        (Rule.readBinary path)
                )
            )


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
    not (String.endsWith "_gradient.png" path)
        && (String.endsWith ".png" path
                || String.endsWith ".webp" path
                || String.endsWith ".jpeg" path
                || String.endsWith ".jpg" path
                || String.endsWith ".git" path
                || String.endsWith ".tif" path
                || String.endsWith ".tiff" path
           )
