module BuildAction exposing (buildAction, getInputs)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Glob as Glob
import Cache exposing (FileOrDirectory)
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Declare
import Elm.Let
import Elm.Op
import Elm.Op.Extra
import FatalError exposing (FatalError)
import Gen.Html
import Gen.Html.Attributes
import Gen.Html.Picture
import Gen.Html.Source
import Gen.List
import Gen.String
import Json.Encode
import List.Extra
import Maybe.Extra
import Parser exposing ((|.), (|=), DeadEnd, Parser)
import Parser.Error
import Parser.Workaround
import Path exposing (Path)
import Result.Extra
import String.Extra
import String.Multiline


type ProcessedFile
    = ProcessedImage
        { original : HashedFileWith { width : Int, height : Int }
        , converted : List (HashedFileWith { width : Int })
        }
    | ProcessedCss Path FileOrDirectory
    | ProcessedSvg (HashedFileWith { width : Int, height : Int })
    | ProcessedFont { family : String, style : Style, weight : Weight } Path FileOrDirectory


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


type alias HashedFileWith a =
    { a
        | filename : Path
        , hash : FileOrDirectory
    }


getInputs : { a | inputDirectory : Path } -> BackendTask FatalError (List ( Path, Cache.Monad FileOrDirectory ))
getInputs config =
    Glob.fromStringWithOptions
        (let
            defaultOptions : Glob.Options
            defaultOptions =
                Glob.defaultOptions
         in
         { defaultOptions | include = Glob.OnlyFiles }
        )
        (Path.toString config.inputDirectory ++ "/**")
        |> BackendTask.andThen
            (\found ->
                found
                    |> List.sort
                    |> List.map Path.path
                    |> Cache.inputs
            )


buildAction : { config | inputDirectory : Path } -> List ( Path, Cache.Monad FileOrDirectory ) -> Cache.Monad FileOrDirectory
buildAction config inputs =
    let
        inputSize : Int
        inputSize =
            List.length inputs

        processFilesTask : Int -> (List ProcessedFile -> Cache.Monad a) -> Cache.Monad a
        processFilesTask parallelism k =
            Cache.do
                (inputs
                    |> List.indexedMap (processFile config inputSize)
                    |> Cache.combineBy parallelism
                )
            <| \v ->
            k (Maybe.Extra.values v)
    in
    Cache.jobs <| \parallelism ->
    processFilesTask parallelism <| \processedFiles ->
    elmCodegen (imagesElmFile processedFiles) <| \imagesElm ->
    elmCodegen (fontsElmFile processedFiles) <| \fontsElm ->
    fontsCssFile processedFiles <| \fontsCss ->
    Cache.do
        (processedFiles
            |> List.concatMap processedFileToFileList
            |> (::) fontsCss
            |> Cache.combine
            |> Cache.withPrefix ("[" ++ String.fromInt inputSize ++ "/" ++ String.fromInt inputSize ++ "]")
        )
    <| \public ->
    Cache.combine
        [ imagesElm
        , fontsElm
        , { filename = Path.path "public", hash = public }
        ]


fontsCssFile :
    List ProcessedFile
    -> ({ filename : Path, hash : FileOrDirectory } -> Cache.Monad a)
    -> Cache.Monad a
fontsCssFile files k =
    let
        content : String
        content =
            files
                |> List.filterMap
                    (\file ->
                        case file of
                            ProcessedFont { family, style, weight } path _ ->
                                Just (String.Multiline.here """
                                    @font-face {
                                        font-family: \"""" ++ family ++ """";
                                        font-style: """ ++ styleToString style ++ """;
                                        font-weight: """ ++ String.fromInt (weightToNumber weight) ++ """;
                                        src: url(\"""" ++ Path.toString path ++ """");
                                    }""")

                            _ ->
                                Nothing
                    )
                |> String.join "\n\n"
    in
    Cache.writeFile content <| \hash ->
    k { filename = Path.path "fonts.css", hash = hash }


fontsElmFile : List ProcessedFile -> Elm.File
fontsElmFile files =
    files
        |> List.filterMap
            (\file ->
                case file of
                    ProcessedFont { family } _ _ ->
                        Just family

                    _ ->
                        Nothing
            )
        |> List.Extra.unique
        |> List.map
            (\family ->
                Elm.declaration (String.replace " " "_" family) (Gen.Html.Attributes.style "font-family" family)
            )
        |> Elm.file [ "Fonts" ]


elmCodegen :
    Elm.File
    -> ({ filename : Path, hash : FileOrDirectory } -> Cache.Monad a)
    -> Cache.Monad a
elmCodegen file k =
    Cache.writeFile file.contents <| \hash ->
    Cache.pipeThrough "elm-format" [ "--stdin" ] hash <| \formatted ->
    k { filename = Path.path ("generated/" ++ file.path), hash = formatted }


imagesElmFile : List ProcessedFile -> Elm.File
imagesElmFile processedFiles =
    processedFiles
        |> List.filterMap
            (\file ->
                case file of
                    ProcessedImage image ->
                        Just (processedImageToDeclaration image)

                    ProcessedSvg image ->
                        Just (processedSvgToDeclaration image)

                    _ ->
                        Nothing
            )
        |> (::) standardFormats.declaration
        |> (::) getSizes_.declaration
        |> (::) toSources.declaration
        |> (::) (toPicture.declaration |> Elm.expose)
        |> Elm.file [ "Images" ]


processedFileToFileList : ProcessedFile -> List { filename : Path, hash : FileOrDirectory }
processedFileToFileList file =
    case file of
        ProcessedImage image ->
            { filename = image.original.filename
            , hash = image.original.hash
            }
                :: List.map
                    (\img ->
                        { filename = img.filename
                        , hash = img.hash
                        }
                    )
                    image.converted

        ProcessedCss path hash ->
            [ { filename = path, hash = hash } ]

        ProcessedSvg original ->
            [ { filename = original.filename, hash = original.hash } ]

        ProcessedFont _ path hash ->
            [ { filename = path, hash = hash } ]


processFile : { config | inputDirectory : Path } -> Int -> Int -> ( Path, Cache.Monad FileOrDirectory ) -> Cache.Monad (Maybe ProcessedFile)
processFile config total index ( path, copyFile ) =
    let
        relative : Path
        relative =
            Path.relativeTo config.inputDirectory path
                |> Path.replaceAll " " "_"

        prefix : String
        prefix =
            "["
                ++ String.padLeft (String.length (String.fromInt total)) '0' (String.fromInt index)
                ++ "/"
                ++ String.fromInt total
                ++ "]"

        image : String -> Cache.Monad (Maybe ProcessedFile)
        image originalExtension =
            Cache.do copyFile <| \copied ->
            Cache.pipeThrough "exiftool" [ "-all=", "-", "-o", "-" ] copied <| \stripped ->
            Cache.pipeThrough "identify" [ "-ping", "-format", "%w %h", "-" ] stripped <| \sizeFile ->
            Cache.withFile sizeFile parseSizeFile <| \sizeData ->
            Cache.each standardFormats.list (convertAndResize ( stripped, originalExtension ) sizeData) <| \converted ->
            Cache.succeed
                ({ original =
                    { width = sizeData.width
                    , height = sizeData.height
                    , filename = relative
                    , hash = stripped
                    }
                 , converted = List.concat converted
                 }
                    |> ProcessedImage
                    |> Just
                )

        parseSizeFile : String -> Cache.Monad { width : Int, height : Int, sizes : List Int }
        parseSizeFile sizeString =
            case String.split " " sizeString |> List.map String.toInt of
                [ Just width, Just height ] ->
                    let
                        sizes : List Int
                        sizes =
                            getSizes width
                    in
                    Cache.succeed
                        { width = width
                        , height = height
                        , sizes = sizes
                        }

                _ ->
                    let
                        msg : String
                        msg =
                            "Could not parse size file: " ++ Json.Encode.encode 0 (Json.Encode.string sizeString)
                    in
                    Cache.fail msg

        convertAndResize :
            ( FileOrDirectory, String )
            ->
                { width : Int
                , height : Int
                , sizes : List Int
                }
            -> { a | extension : String }
            -> Cache.Monad (List (HashedFileWith { width : Int }))
        convertAndResize ( stripped, originalExtension ) sizeData { extension } =
            let
                convertedFilename : Path
                convertedFilename =
                    Path.replaceExtensionWith extension relative
            in
            convertTo extension ( stripped, originalExtension ) <| \converted ->
            let
                doResize : Int -> Cache.Monad (HashedFileWith { width : Int })
                doResize w =
                    (if w == sizeData.width then
                        Cache.do (Cache.succeed converted)

                     else
                        Cache.pipeThrough "magick" [ "-", "-resize", String.fromInt w ++ "x" ++ String.fromInt sizeData.height, "-" ] converted
                    )
                    <| \resized ->
                    Cache.succeed
                        { width = w
                        , filename =
                            convertedFilename
                                |> Path.appendToFilename ("-" ++ String.fromInt w)
                        , hash = resized
                        }
            in
            Cache.each sizeData.sizes doResize Cache.succeed
    in
    (case Path.extension path of
        Just "webp" ->
            image "webp"

        Just "jpg" ->
            image "jpg"

        Just "jpeg" ->
            image "jpeg"

        Just "png" ->
            image "png"

        Just "ttf" ->
            Cache.do copyFile <| \hash ->
            processFont relative hash

        Just "otf" ->
            Cache.do copyFile <| \hash ->
            processFont relative hash

        Just "svg" ->
            Cache.do copyFile <| \hash ->
            Cache.withFile hash getSvgSize <| \size ->
            Cache.succeed
                (Just
                    (ProcessedSvg
                        { filename = relative
                        , hash = hash
                        , width = size.width
                        , height = size.height
                        }
                    )
                )

        Just "zip" ->
            -- Ignore
            Cache.succeed Nothing

        Just "txt" ->
            -- Ignore
            Cache.succeed Nothing

        Just "md" ->
            -- Ignore
            Cache.succeed Nothing

        Just "css" ->
            Cache.do copyFile <| \hash ->
            Cache.succeed (Just (ProcessedCss relative hash))

        _ ->
            Cache.succeed Nothing
     -- Cache.fail ("Don't know how to process " ++ Path.toString path)
    )
        |> Cache.timed
            ("Processing " ++ Path.toString path)
            ("Processed  " ++ Path.toString path)
        |> Cache.withPrefix prefix


getSvgSize : String -> Cache.Monad { width : Int, height : Int }
getSvgSize input =
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
    -> List DeadEnd
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


processFont : Path -> FileOrDirectory -> Cache.Monad (Maybe ProcessedFile)
processFont relative hash =
    let
        readFontData : String -> Cache.Monad ( String, { style : Style, weight : Weight } )
        readFontData familyAndStyle =
            case String.split " || " familyAndStyle of
                [ family, styleAndWeight ] ->
                    parseStyleAndWeight styleAndWeight
                        |> Result.map
                            (\parsed ->
                                ( family, parsed )
                                    |> Cache.succeed
                            )
                        |> Result.mapError Cache.fail
                        |> Result.Extra.merge

                _ ->
                    Cache.fail ("Failed to parse family and style: " ++ familyAndStyle)
    in
    Cache.commandWithFile "fc-scan" [ "--format", "%{family[0]} || %{style[0]}" ] hash <| \familyAndStyleFile ->
    Cache.withFile familyAndStyleFile readFontData <| \( family, { style, weight } ) ->
    Cache.succeed (Just (ProcessedFont { family = family, style = style, weight = weight } relative hash))


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


minSize : number
minSize =
    50


getSizes : Int -> List Int
getSizes width =
    let
        go : Int -> List Int -> List Int
        go factor acc =
            let
                w : Int
                w =
                    width // factor
            in
            if w >= minSize then
                go (factor * 2) (w :: acc)

            else
                List.reverse acc
    in
    go 1 []


getSizes_ : Elm.Declare.Function (Elm.Expression -> Elm.Expression)
getSizes_ =
    Elm.Declare.fn "getSizes" (Elm.Arg.varWith "width" Elm.Annotation.int) <| \width ->
    Elm.Let.letIn identity
        |> Elm.Let.fn2 "go"
            (Elm.Arg.varWith "factor" Elm.Annotation.int)
            (Elm.Arg.varWith "acc" (Elm.Annotation.list Elm.Annotation.int))
            (\factor acc ->
                Elm.Let.letIn identity
                    |> Elm.Let.value "w" (Elm.Op.intDivide width factor)
                    |> Elm.Let.withBody
                        (\w ->
                            Elm.ifThen (Elm.Op.gte w (Elm.int minSize))
                                (Elm.apply (Elm.val "go") [ Elm.Op.multiply factor (Elm.int 2), Elm.Op.cons w acc ])
                                (Gen.List.call_.reverse acc)
                        )
                    |> Elm.withType (Elm.Annotation.list Elm.Annotation.int)
            )
        |> Elm.Let.withBody
            (\go ->
                go (Elm.int 1) (Elm.list [])
                    |> Elm.withType (Elm.Annotation.list Elm.Annotation.int)
            )


convertTo :
    String
    -> ( FileOrDirectory, String )
    -> (FileOrDirectory -> Cache.Monad a)
    -> Cache.Monad a
convertTo format ( cached, originalExtension ) k =
    if originalExtension == format then
        k cached

    else
        Cache.pipeThrough "magick" [ "-", format ++ ":-" ] cached k


standardFormats :
    { list : List { format : Elm.Expression, extension : String }
    , declaration : Elm.Declaration
    , value : Elm.Expression
    }
standardFormats =
    let
        list : List { format : Elm.Expression, extension : String }
        list =
            [ { format = Gen.Html.Source.make_.jPEG_XL, extension = "jxl" }
            , { format = Gen.Html.Source.make_.aVIF, extension = "avif" }
            , { format = Gen.Html.Source.make_.webP, extension = "webp" }
            , { format = Gen.Html.Source.make_.jPEG, extension = "jpg" }
            ]

        declaration : Elm.Declare.Value
        declaration =
            list
                |> List.map
                    (\{ format, extension } ->
                        Elm.record
                            [ ( "format", format )
                            , ( "extension", Elm.string extension )
                            ]
                    )
                |> Elm.list
                |> Elm.Declare.value "standardFormats"
    in
    { list = list
    , declaration = declaration.declaration
    , value = declaration.value
    }


processedImageToDeclaration :
    { image
        | original : HashedFileWith { width : Int, height : Int }
    }
    -> Elm.Declaration
processedImageToDeclaration { original } =
    let
        name : String
        name =
            toVariableName original.filename

        attrsAnnotation : Elm.Annotation.Annotation
        attrsAnnotation =
            Elm.Annotation.list (Gen.Html.annotation_.attribute (Elm.Annotation.var "msg"))
    in
    Elm.declaration name
        (Elm.fn
            (Elm.Arg.varWith "attrs" attrsAnnotation)
            (\attrs ->
                let
                    dir : String
                    dir =
                        Path.toString (Path.directory original.filename)
                in
                toPicture.call attrs
                    (Elm.string
                        (if String.isEmpty dir then
                            Path.filenameWithoutExtension original.filename

                         else
                            dir ++ "/" ++ Path.filenameWithoutExtension original.filename
                        )
                    )
                    (Path.extension original.filename
                        |> Maybe.withDefault ""
                        |> Elm.string
                    )
                    (Elm.int original.width)
                    (Elm.int original.height)
            )
            |> Elm.withType Elm.Annotation.unit
            |> Elm.withType
                (Elm.Annotation.function
                    [ attrsAnnotation ]
                    (Gen.Html.annotation_.html (Elm.Annotation.var "msg"))
                )
        )
        |> Elm.expose


processedSvgToDeclaration :
    HashedFileWith { width : Int, height : Int }
    -> Elm.Declaration
processedSvgToDeclaration original =
    let
        name : String
        name =
            toVariableName original.filename

        attrsAnnotation : Elm.Annotation.Annotation
        attrsAnnotation =
            Elm.Annotation.list (Gen.Html.annotation_.attribute (Elm.Annotation.var "msg"))
    in
    Elm.declaration name
        (Elm.fn
            (Elm.Arg.varWith "attrs" attrsAnnotation)
            (\attrs ->
                Gen.Html.call_.img
                    (Elm.Op.cons (Gen.Html.Attributes.src (Path.toString original.filename))
                        (Elm.Op.cons (Gen.Html.Attributes.width original.width)
                            (Elm.Op.cons (Gen.Html.Attributes.height original.height)
                                attrs
                            )
                        )
                    )
                    (Elm.list [])
            )
            |> Elm.withType
                (Elm.Annotation.function
                    [ attrsAnnotation ]
                    (Gen.Html.annotation_.html (Elm.Annotation.var "msg"))
                )
        )
        |> Elm.expose


toVariableName : Path -> String
toVariableName path =
    let
        dir : String
        dir =
            Path.toString (Path.directory path)

        nameWithoutExtension : String
        nameWithoutExtension =
            if String.isEmpty dir then
                Path.filenameWithoutExtension path

            else
                dir ++ "/" ++ Path.filenameWithoutExtension path

        stripLeadingUnderscores i =
            if String.startsWith "_" i then
                stripLeadingUnderscores (String.dropLeft 1 i)

            else
                i
    in
    nameWithoutExtension
        |> String.replace "/" "_"
        |> String.replace " " "_"
        |> String.replace "-" "_"
        |> stripLeadingUnderscores
        |> String.Extra.decapitalize


toSources : Elm.Declare.Function (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
toSources =
    Elm.Declare.fn3 "toSources"
        (Elm.Arg.varWith "base" Elm.Annotation.string)
        (Elm.Arg.varWith "originalWidth" Elm.Annotation.int)
        (Elm.Arg.varWith "config"
            (Elm.Annotation.record
                [ ( "extension", Elm.Annotation.string )
                , ( "format", Gen.Html.Source.annotation_.imageType )
                ]
            )
        )
    <| \base originalWidth config ->
    getSizes_.call originalWidth
        |> Gen.List.call_.map
            (Elm.fn (Elm.Arg.varWith "w" Elm.Annotation.int) <| \w ->
            Elm.record
                [ ( "url"
                  , Elm.Op.Extra.appends
                        base
                        [ Elm.string "-"
                        , Gen.String.call_.fromInt w
                        , Elm.string "."
                        , Elm.get "extension" config
                        ]
                  )
                , ( "width", Elm.maybe (Just w) )
                ]
            )
        |> Gen.Html.Source.call_.fromImagesAndWidths
        |> Gen.Html.Source.withType (Elm.get "format" config)
        |> Elm.withType (Gen.Html.Source.annotation_.source Gen.Html.Source.annotation_.withWidths)


toPicture : Elm.Declare.Function (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
toPicture =
    Elm.Declare.fn5 "toPicture"
        (Elm.Arg.varWith "attrs"
            (Elm.Annotation.list (Gen.Html.annotation_.attribute (Elm.Annotation.var "msg")))
        )
        (Elm.Arg.varWith "base" Elm.Annotation.string)
        (Elm.Arg.varWith "originalExtension" Elm.Annotation.string)
        (Elm.Arg.varWith "originalWidth" Elm.Annotation.int)
        (Elm.Arg.varWith "originalHeight" Elm.Annotation.int)
    <| \attrs base originalExtension originalWidth originalHeight ->
    Gen.Html.Picture.call_.picture
        (Elm.Op.cons
            (Gen.Html.Attributes.call_.width originalWidth)
            (Elm.Op.cons
                (Gen.Html.Attributes.call_.height originalHeight)
                attrs
            )
        )
        (Elm.record
            [ ( "sources"
              , standardFormats.value
                    |> Gen.List.call_.map
                        (Elm.functionReduced "format" <|
                            toSources.call
                                base
                                originalWidth
                        )
              )
            , ( "src", Elm.Op.append (Elm.Op.append base (Elm.string ".")) originalExtension )
            , ( "alt", Elm.maybe Nothing )
            ]
        )
        |> Elm.withType (Gen.Html.annotation_.html (Elm.Annotation.var "msg"))
