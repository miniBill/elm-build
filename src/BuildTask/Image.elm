module BuildTask.Image exposing (getSize, getSvgSize, stripMetadata)

import Ansi.Color
import BackendTask.File as File
import BackendTask.Stream as Stream
import BuildTask exposing (BuildTask, Command, FileOrDirectory)
import BuildTask.Unsafe
import FatalError exposing (FatalError)
import Parser exposing ((|.), (|=), Parser)
import Parser.Error
import Parser.Workaround


getSvgSize : FileOrDirectory -> BuildTask tools { fatal : FatalError, recoverable : File.FileReadError String } { width : Int, height : Int }
getSvgSize hash =
    BuildTask.withFile hash parseSvgSize


parseSvgSize : String -> BuildTask tools { fatal : FatalError, recoverable : File.FileReadError String } { width : Int, height : Int }
parseSvgSize input =
    case Parser.run viewBoxParser input of
        Ok { width, height } ->
            BuildTask.succeed { width = width, height = height }

        Err e ->
            let
                msg : String
                msg =
                    errorToString input e
            in
            BuildTask.fail
                { fatal = FatalError.fromString msg
                , recoverable = File.DecodingError msg
                }


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


stripMetadata : FileOrDirectory -> BuildTask { tools | exiftool : Command } { fatal : FatalError, recoverable : Stream.Error () String } FileOrDirectory
stripMetadata hash =
    BuildTask.Unsafe.pipeThrough .exiftool [ "-all=", "-", "-o", "-" ] hash


getSize : FileOrDirectory -> BuildTask { tools | identify : Command } { fatal : FatalError, recoverable : Stream.Error () String } FileOrDirectory
getSize file =
    BuildTask.Unsafe.pipeThrough .identify [ "-ping", "-format", "%w %h", "-" ] file
