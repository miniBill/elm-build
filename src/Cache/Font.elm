module Cache.Font exposing (Data, Style(..), Weight(..), parse, styleToString, weightToNumber)

import Cache exposing (FileOrDirectory)
import Cache.Do as Do


type alias Data =
    { family : String
    , style : Style
    , weight : Weight
    }


type Style
    = StyleNormal
    | StyleItalic
    | StyleOblique


styleToString : Style -> String
styleToString style =
    case style of
        StyleNormal ->
            "normal"

        StyleItalic ->
            "italic"

        StyleOblique ->
            "oblique"


type Weight
    = WeightThin -- (Hairline)
    | WeightExtraLight -- (Ultra Light)
    | WeightLight
    | WeightNormal -- (Regular)
    | WeightMedium
    | WeightSemiBold -- (Demi Bold)
    | WeightBold
    | WeightExtraBold -- (Ultra Bold)
    | WeightBlack -- (Heavy)
    | WeightExtraBlack -- (Ultra Black)


weightToNumber : Weight -> number
weightToNumber weight =
    case weight of
        WeightThin ->
            100

        WeightExtraLight ->
            200

        WeightLight ->
            300

        WeightNormal ->
            400

        WeightMedium ->
            500

        WeightSemiBold ->
            600

        WeightBold ->
            700

        WeightExtraBold ->
            800

        WeightBlack ->
            900

        WeightExtraBlack ->
            950


parse : FileOrDirectory -> Cache.Monad Data
parse hash =
    let
        readFontData : String -> Cache.Monad Data
        readFontData familyAndStyle =
            case String.split " || " familyAndStyle of
                [ family, styleAndWeight ] ->
                    case parseStyleAndWeight styleAndWeight of
                        Ok { style, weight } ->
                            { family = family, style = style, weight = weight }
                                |> Cache.succeed

                        Err e ->
                            Cache.fail e

                _ ->
                    Cache.fail ("Failed to parse family and style: " ++ familyAndStyle)
    in
    Do.commandWithFile "fc-scan" [ "--format", "%{family[0]} || %{style[0]}" ] hash <| \familyAndStyleFile ->
    Cache.withFile familyAndStyleFile readFontData


parseStyleAndWeight : String -> Result String { style : Style, weight : Weight }
parseStyleAndWeight styleAndWeight =
    styleAndWeight
        |> String.split " "
        |> List.foldl
            (\e ->
                Result.andThen
                    (\a ->
                        case e of
                            "Normal" ->
                                Ok a

                            "Italic" ->
                                Ok { a | style = StyleItalic }

                            "Oblique" ->
                                Ok { a | style = StyleOblique }

                            "Black" ->
                                Ok { a | weight = WeightBlack }

                            "Bold" ->
                                Ok { a | weight = WeightBold }

                            "ExtraBold" ->
                                Ok { a | weight = WeightExtraBold }

                            "ExtraLight" ->
                                Ok { a | weight = WeightExtraLight }

                            "Light" ->
                                Ok { a | weight = WeightLight }

                            "Medium" ->
                                Ok { a | weight = WeightMedium }

                            "Regular" ->
                                Ok a

                            "SemiBold" ->
                                Ok { a | weight = WeightSemiBold }

                            "Thin" ->
                                Ok { a | weight = WeightThin }

                            _ ->
                                Err ("Failed to parse style fragment: \"" ++ e ++ "\"")
                    )
            )
            (Ok { style = StyleNormal, weight = WeightNormal })
