module Buildfile exposing (buildAction, getInputs)

import BackendTask exposing (BackendTask)
import BackendTask.Glob as Glob
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Do as Do
import BuildTask.Elm as Elm
import BuildTask.Font as Font
import BuildTask.Image as Image
import BuildTask.Unsafe as Unsafe
import BuildTask.Unsafe.Do as Do
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
import Path exposing (Path)
import String.Extra


type ProcessedFile
    = ProcessedImage
        { original : HashedFileWith { width : Int, height : Int }
        , converted : List (HashedFileWith { width : Int })
        }
    | ProcessedCss (HashedFileWith {})
    | ProcessedSvg (HashedFileWith { width : Int, height : Int })
    | ProcessedFont (HashedFileWith Font.Data)


type alias HashedFileWith a =
    { a
        | filename : Path
        , hash : FileOrDirectory
    }


getInputs : { a | inputDirectory : Path } -> BackendTask FatalError (List ( Path, BuildTask FileOrDirectory ))
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
                    |> BuildTask.inputs
            )


type T4 a b c d
    = T4 a b c d


buildAction : { config | inputDirectory : Path } -> List ( Path, BuildTask FileOrDirectory ) -> BuildTask FileOrDirectory
buildAction config inputs =
    let
        inputSize : Int
        inputSize =
            List.length inputs
    in
    BuildTask.do
        (Do.jobs <| \parallelism ->
        inputs
            |> List.indexedMap (processFile config inputSize)
            |> BuildTask.combineBy parallelism
            |> BuildTask.map Maybe.Extra.values
        )
    <| \processedFiles ->
    let
        fontFiles : List (HashedFileWith Font.Data)
        fontFiles =
            List.filterMap asFont processedFiles

        imageFiles :
            List
                { original : HashedFileWith { width : Int, height : Int }
                , converted : List (HashedFileWith { width : Int })
                }
        imageFiles =
            List.filterMap asImage processedFiles

        publicFolder : BuildTask FileOrDirectory
        publicFolder =
            Do.writeFile (Font.toCssFile fontFiles) <| \fontsCssHash ->
            ({ filename = Path.path "fonts.css"
             , hash = fontsCssHash
             }
                :: List.concatMap processedFileToFileList processedFiles
            )
                |> BuildTask.combine
                |> BuildTask.withPrefix ("[" ++ String.fromInt inputSize ++ "/" ++ String.fromInt inputSize ++ "]")
    in
    Do.map4 T4
        (imagesElmFile processedFiles)
        (Elm.codegen (fontsElmFile fontFiles))
        (imagesSizesFile imageFiles)
        publicFolder
    <| \(T4 imagesElm fontsElm imageSizes public) ->
    BuildTask.combine
        [ { filename = Path.path "generated/Images.elm", hash = imagesElm }
        , fontsElm
        , { filename = Path.path "image-sizes", hash = imageSizes }
        , { filename = Path.path "public", hash = public }
        ]


asImage :
    ProcessedFile
    ->
        Maybe
            { original : HashedFileWith { width : Int, height : Int }
            , converted : List (HashedFileWith { width : Int })
            }
asImage file =
    case file of
        ProcessedImage data ->
            Just data

        ProcessedCss _ ->
            Nothing

        ProcessedSvg _ ->
            Nothing

        ProcessedFont _ ->
            Nothing


asFont : ProcessedFile -> Maybe (HashedFileWith Font.Data)
asFont file =
    case file of
        ProcessedFont data ->
            Just data

        _ ->
            Nothing


imagesSizesFile :
    List
        { a
            | original : HashedFileWith { width : Int, height : Int }
        }
    -> BuildTask FileOrDirectory
imagesSizesFile processedFiles =
    let
        content : String
        content =
            processedFiles
                |> List.map
                    (\{ original } ->
                        let
                            name : String
                            name =
                                Path.toString original.filename
                                    |> String.replace " " "_"
                        in
                        name
                            ++ ": "
                            ++ String.fromInt original.width
                            ++ "x"
                            ++ String.fromInt original.height
                    )
                |> String.join "\n"
    in
    BuildTask.writeFile content


fontsElmFile : List (HashedFileWith Font.Data) -> Elm.File
fontsElmFile files =
    files
        |> List.map .family
        |> List.Extra.unique
        |> List.map
            (\family ->
                Elm.declaration (String.replace " " "_" family) (Gen.Html.Attributes.style "font-family" family)
            )
        |> Elm.file [ "Fonts" ]


imagesElmFile : List ProcessedFile -> BuildTask FileOrDirectory
imagesElmFile list =
    list
        |> List.filterMap
            (\processedFile ->
                case processedFile of
                    ProcessedImage { original } ->
                        Just
                            { svg = False
                            , filename = original.filename
                            , hash = original.hash
                            , width = original.width
                            , height = original.height
                            }

                    ProcessedSvg original ->
                        Just
                            { svg = True
                            , filename = original.filename
                            , hash = original.hash
                            , width = original.width
                            , height = original.height
                            }

                    ProcessedCss _ ->
                        Nothing

                    ProcessedFont _ ->
                        Nothing
            )
        |> Unsafe.named "imagesElmFile"
            encodeProcessedFiles
            (\processedFiles ->
                let
                    file : Elm.File
                    file =
                        processedFiles
                            |> List.map
                                (\processedFile ->
                                    if processedFile.svg then
                                        processedSvgToDeclaration processedFile

                                    else
                                        processedImageToDeclaration processedFile
                                )
                            |> (::) standardFormats.declaration
                            |> (::) getSizes_.declaration
                            |> (::) toSources.declaration
                            |> (::) (toPicture.declaration |> Elm.expose)
                            |> Elm.file [ "Images" ]
                in
                Do.writeFile file.contents <| \hash ->
                Elm.format hash
            )


encodeProcessedFiles :
    List
        { svg : Bool
        , filename : Path
        , hash : FileOrDirectory
        , width : Int
        , height : Int
        }
    -> { files : List FileOrDirectory, additionalData : List String }
encodeProcessedFiles processedFiles =
    let
        ( files, additionalData ) =
            processedFiles
                |> List.map
                    (\{ filename, hash } ->
                        -- CORRECTNESS: if svg, width or height change then hash
                        -- will change too so we don't need to track them separately
                        ( hash, Path.toString filename )
                    )
                |> List.unzip
    in
    { files = files, additionalData = additionalData }


processedFileToFileList : ProcessedFile -> List { filename : Path, hash : FileOrDirectory }
processedFileToFileList file =
    let
        extract : HashedFileWith a -> HashedFileWith {}
        extract original =
            { filename = original.filename
            , hash = original.hash
            }
    in
    case file of
        ProcessedImage image ->
            extract image.original
                :: List.map extract image.converted

        ProcessedCss data ->
            [ extract data ]

        ProcessedSvg data ->
            [ extract data ]

        ProcessedFont data ->
            [ extract data ]


processFile : { config | inputDirectory : Path } -> Int -> Int -> ( Path, BuildTask FileOrDirectory ) -> BuildTask (Maybe ProcessedFile)
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

        image : String -> BuildTask (Maybe ProcessedFile)
        image originalExtension =
            BuildTask.do copyFile <| \copied ->
            BuildTask.do (Image.stripMetadata copied) <| \stripped ->
            BuildTask.do (Image.getSize stripped) <| \sizeFile ->
            Do.withFile sizeFile parseSizeFile <| \sizeData ->
            Do.each standardFormats.list (convertAndResize ( stripped, originalExtension ) sizeData) <| \converted ->
            BuildTask.succeed
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

        parseSizeFile : String -> BuildTask { width : Int, height : Int, sizes : List Int }
        parseSizeFile sizeString =
            case String.split " " sizeString |> List.map String.toInt of
                [ Just width, Just height ] ->
                    let
                        sizes : List Int
                        sizes =
                            getSizes width
                    in
                    BuildTask.succeed
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
                    BuildTask.fail msg

        convertAndResize :
            ( FileOrDirectory, String )
            ->
                { width : Int
                , height : Int
                , sizes : List Int
                }
            -> { a | extension : String }
            -> BuildTask (List (HashedFileWith { width : Int }))
        convertAndResize ( stripped, originalExtension ) sizeData { extension } =
            let
                convertedFilename : Path
                convertedFilename =
                    Path.replaceExtensionWith extension relative
            in
            convertTo extension ( stripped, originalExtension ) <| \converted ->
            let
                doResize : Int -> BuildTask (HashedFileWith { width : Int })
                doResize w =
                    (if w == sizeData.width then
                        BuildTask.succeed converted

                     else
                        Unsafe.pipeThrough "magick" [ "-", "-resize", String.fromInt w ++ "x" ++ String.fromInt sizeData.height, "-" ] converted
                    )
                        |> BuildTask.map
                            (\resized ->
                                { width = w
                                , filename =
                                    convertedFilename
                                        |> Path.appendToFilename ("-" ++ String.fromInt w)
                                , hash = resized
                                }
                            )
            in
            BuildTask.each sizeData.sizes doResize

        font () =
            BuildTask.do copyFile <| \hash ->
            BuildTask.map
                (\fontData ->
                    Just
                        (ProcessedFont
                            { style = fontData.style
                            , weight = fontData.weight
                            , family = fontData.family
                            , filename = relative
                            , hash = hash
                            }
                        )
                )
                (Font.parse hash)
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
            font ()

        Just "otf" ->
            font ()

        Just "svg" ->
            BuildTask.do copyFile <| \hash ->
            BuildTask.do (Image.getSvgSize hash) <| \size ->
            BuildTask.succeed
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
            BuildTask.succeed Nothing

        Just "txt" ->
            -- Ignore
            BuildTask.succeed Nothing

        Just "md" ->
            -- Ignore
            BuildTask.succeed Nothing

        Just "css" ->
            BuildTask.do copyFile <| \hash ->
            BuildTask.succeed (Just (ProcessedCss { filename = relative, hash = hash }))

        _ ->
            -- Cache.fail ("Don't know how to process " ++ Path.toString path)
            BuildTask.succeed Nothing
    )
        |> BuildTask.timed
            ("Processing " ++ Path.toString path)
            ("Processed  " ++ Path.toString path)
        |> BuildTask.withPrefix prefix


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
    -> (FileOrDirectory -> BuildTask a)
    -> BuildTask a
convertTo format ( cached, originalExtension ) k =
    if originalExtension == format then
        k cached

    else
        Do.pipeThrough "magick" [ "-", format ++ ":-" ] cached k


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
    HashedFileWith { a | width : Int, height : Int }
    -> Elm.Declaration
processedImageToDeclaration original =
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
    HashedFileWith { a | width : Int, height : Int }
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
