module BuildTask.Font exposing (Data, FontError, Style(..), Weight(..), parse, styleToString, toCssFile, weightToNumber)

import BackendTask.File as File
import BackendTask.Stream as Stream
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Unsafe as Unsafe
import FatalError exposing (FatalError)
import Path.Posix as Path exposing (Path)
import String.Multiline
import Utils


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


parse : { tools | fc_scan : BuildTask.Command } -> FileOrDirectory -> BuildTask { recoverable : FontError, fatal : FatalError } Data
parse { fc_scan } hash =
    let
        readFontData : String -> BuildTask { fatal : FatalError, recoverable : FontError } Data
        readFontData familyAndStyle =
            case String.split " || " familyAndStyle of
                [ family, styleAndWeight ] ->
                    case parseStyleAndWeight styleAndWeight of
                        Ok { style, weight } ->
                            { family = String.replace ":" " " family
                            , style = style
                            , weight = weight
                            }
                                |> BuildTask.succeed

                        Err e ->
                            BuildTask.fail e

                _ ->
                    FatalError.recoverable
                        { title = "Failed to parse family and style"
                        , body = "Failed to parse " ++ Utils.escape familyAndStyle
                        }
                        (FailedToParseFamilyAndStyle familyAndStyle)
                        |> BuildTask.fail
    in
    BuildTask.doWithError (Unsafe.commandWithFile fc_scan [ "--format", "%{family[0]} || %{style[0]}" ] hash) FailedToRunFcScan <| \familyAndStyleFile ->
    BuildTask.doWithError (BuildTask.withFile familyAndStyleFile BuildTask.succeed) FailedToReadFile <| \familyAndStyle ->
    readFontData familyAndStyle


type FontError
    = UnrecognizedStyleOrWeight String
    | FailedToParseFamilyAndStyle String
    | FailedToReadFile (File.FileReadError Never)
    | FailedToRunFcScan (Stream.Error Int String)


parseStyleAndWeight : String -> Result { fatal : FatalError, recoverable : FontError } { style : Style, weight : Weight }
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
                                FatalError.recoverable
                                    { title = "Unrecognized style of weight"
                                    , body = "Unrecognized style or weight: " ++ Utils.escape e
                                    }
                                    (UnrecognizedStyleOrWeight e)
                                    |> Err
                    )
            )
            (Ok { style = StyleNormal, weight = WeightNormal })


toCssFile :
    List
        { a
            | family : String
            , style : Style
            , weight : Weight
            , filename : Path Path.Relative Path.File
        }
    -> String
toCssFile files =
    files
        |> List.map
            (\{ family, style, weight, filename } ->
                String.Multiline.here ("""
                @font-face {
                    font-family: \"""" ++ family ++ """";
                    font-style: """ ++ styleToString style ++ """;
                    font-weight: """ ++ String.fromInt (weightToNumber weight) ++ """;
                    src: url(\"""" ++ Path.toString filename ++ """");
                }""")
            )
        |> String.join "\n\n"
