module Cache.Image exposing (getSize, getSvgSize, stripMetadata)

import Ansi.Color
import Cache exposing (FileOrDirectory)
import Parser exposing ((|.), (|=), Parser)
import Parser.Error
import Parser.Workaround


getSvgSize : FileOrDirectory -> Cache.Monad { width : Int, height : Int }
getSvgSize hash =
    Cache.withFile hash parseSvgSize


parseSvgSize : String -> Cache.Monad { width : Int, height : Int }
parseSvgSize input =
    case Parser.run viewBoxParser input of
        Ok { width, height } ->
            Cache.succeed { width = width, height = height }

        Err e ->
            Cache.fail (errorToString input e)


viewBoxParser : Parser { x : Int, y : Int, width : Int, height : Int }
viewBoxParser =
    Parser.succeed (\x y w h -> { x = x, y = y, width = w, height = h })
        |. doctypeParser
        |. Parser.spaces
        |. Parser.symbol "<svg"
        |. Parser.Workaround.chompUntilAfter "viewBox=\""
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |. Parser.symbol "\""


doctypeParser : Parser ()
doctypeParser =
    Parser.oneOf
        [ Parser.succeed ()
            |. Parser.symbol "<?xml"
            |. Parser.Workaround.chompUntilAfter "?>"
        , Parser.succeed ()
        ]


errorToString :
    String
    -> List Parser.DeadEnd
    -> String
errorToString src deadEnds =
    Parser.Error.renderError
        { text = identity
        , formatContext = Ansi.Color.fontColor Ansi.Color.cyan
        , formatCaret = Ansi.Color.fontColor Ansi.Color.red
        , newline = "\n"
        , linesOfExtraContext = 3
        }
        Parser.Error.forParser
        src
        deadEnds
        |> String.concat


stripMetadata : FileOrDirectory -> Cache.Monad FileOrDirectory
stripMetadata hash =
    Cache.pipeThrough "exiftool" [ "-all=", "-", "-o", "-" ] hash


getSize : FileOrDirectory -> Cache.Monad FileOrDirectory
getSize file =
    Cache.pipeThrough "identify" [ "-ping", "-format", "%w %h", "-" ] file
