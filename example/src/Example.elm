module Example exposing (HashedFileWith, Inputs, Tools, buildFile, image, standardFormats)

import BackendTask exposing (BackendTask)
import BackendTask.Customs
import BackendTask.File.Extra
import BackendTask.Glob as Glob
import Build exposing (BuildFile)
import BuildTask exposing (BuildTask, Command, FileOrDirectory)
import BuildTask.Do as Do
import BuildTask.Elm as Elm
import BuildTask.Font as Font
import BuildTask.Image as Image
import BuildTask.Unsafe as Unsafe
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
import List.Extra
import Maybe.Extra
import Pages.Script as Script
import Path.Posix as Path exposing (Path)
import Result.Extra
import String.Extra
import Utils


buildFile : BuildFile { inputPath : String } Inputs
buildFile =
    { getInputs = getInputs
    , buildAction = buildAction
    }


type alias Tools =
    { elm_format : Command
    , exiftool : Command
    , fc_scan : Command
    , identify : Command
    , magick : Command
    }


type alias Inputs =
    { inputPath : Path Path.Absolute Path.Directory
    , files :
        List
            ( Path Path.Absolute Path.File
            , BuildTask FatalError FileOrDirectory
            )
    }


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
        | filename : Path Path.Relative Path.File
        , hash : FileOrDirectory
    }


getInputs :
    { config
        | inputPath : String
        , buildPath : String
        , debug : Bool
    }
    -> BackendTask FatalError Inputs
getInputs config =
    BackendTask.File.Extra.resolveDirectory config.inputPath
        |> BackendTask.andThen
            (\inputPath ->
                let
                    filesTask :
                        BackendTask
                            FatalError
                            (List ( Path Path.Absolute Path.File, BuildTask e FileOrDirectory ))
                    filesTask =
                        Glob.fromStringWithOptions
                            (let
                                defaultOptions : Glob.Options
                                defaultOptions =
                                    Glob.defaultOptions
                             in
                             { defaultOptions | include = Glob.OnlyFiles }
                            )
                            (Path.toString inputPath ++ "/**")
                            |> BackendTask.andThen
                                (\found ->
                                    found
                                        |> List.sort
                                        |> Result.Extra.combineMap
                                            (\filename ->
                                                case Path.parseAbsoluteFile filename of
                                                    Just file ->
                                                        Ok file

                                                    Nothing ->
                                                        Err (FatalError.fromString ("Invalid filename " ++ filename))
                                            )
                                        |> BackendTask.fromResult
                                )
                            |> BackendTask.andThen (BuildTask.inputs config)
                in
                BackendTask.map (Inputs inputPath) filesTask
            )


type T4 a b c d
    = T4 a b c d


getTools : BuildTask FatalError Tools
getTools =
    BuildTask.succeed Tools
        |> BuildTask.andMap (BuildTask.which "elm-format")
        |> BuildTask.andMap (BuildTask.which "exiftool")
        |> BuildTask.andMap (BuildTask.which "fc-scan")
        |> BuildTask.andMap (BuildTask.which "identify")
        |> BuildTask.andMap (BuildTask.which "magick")


buildAction : Inputs -> BuildTask FatalError FileOrDirectory
buildAction inputs =
    let
        inputSize : Int
        inputSize =
            List.length inputs.files
    in
    BuildTask.do getTools <| \tools ->
    BuildTask.do
        (inputs.files
            |> List.indexedMap (processFile tools inputs inputSize)
            |> BuildTask.combine
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

        publicFolder : BuildTask { fatal : FatalError, recoverable : Script.Error } FileOrDirectory
        publicFolder =
            Do.writeFile (Font.toCssFile fontFiles) <| \fontsCssHash ->
            ({ filename = Path.parseRelativeFile "fonts.css" |> trustMe
             , hash = fontsCssHash
             }
                :: List.concatMap processedFileToFileList processedFiles
            )
                |> BuildTask.combineInto
                |> BuildTask.withPrefix ("[" ++ String.fromInt inputSize ++ "/" ++ String.fromInt inputSize ++ "]")
    in
    Do.map4 T4
        (imagesElmFile tools processedFiles)
        (fontsElmFile fontFiles |> BuildTask.andThen (\file -> file |> Elm.codegen tools |> BuildTask.allowFatal))
        (imagesSizesFile imageFiles)
        (publicFolder |> BuildTask.allowFatal)
    <| \(T4 imagesElm fontsElm imageSizes public) ->
    BuildTask.combineInto
        [ { filename = Path.parseRelativeFile "generated/Images.elm" |> trustMe, hash = imagesElm }
        , fontsElm
        , { filename = Path.parseRelativeFile "image-sizes" |> trustMe, hash = imageSizes }
        , { filename = Path.parseRelativeFile "public" |> trustMe, hash = public }
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
    -> BuildTask FatalError FileOrDirectory
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
        |> BuildTask.allowFatal


fontsElmFile :
    List (HashedFileWith Font.Data)
    ->
        BuildTask
            FatalError
            { path : Path Path.Relative Path.File
            , contents : String
            , warnings :
                List
                    { declaration : String
                    , warning : String
                    }
            }
fontsElmFile files =
    let
        raw : Elm.File
        raw =
            files
                |> List.map .family
                |> List.Extra.unique
                |> List.map
                    (\family ->
                        Elm.declaration (String.replace " " "_" family) (Gen.Html.Attributes.style "font-family" family)
                    )
                |> Elm.file [ "Fonts" ]
    in
    case Path.parseRelativeFile raw.path of
        Nothing ->
            BuildTask.fail (FatalError.fromString ("Invalid filename: " ++ raw.path))

        Just path ->
            { path = path
            , contents = raw.contents
            , warnings = raw.warnings
            }
                |> BuildTask.succeed


imagesElmFile : Tools -> List ProcessedFile -> BuildTask FatalError FileOrDirectory
imagesElmFile tools list =
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
                    fromFilesResult : Maybe (List Elm.Declaration)
                    fromFilesResult =
                        Maybe.Extra.combineMap
                            (\processedFile ->
                                if processedFile.svg then
                                    processedSvgToDeclaration processedFile

                                else
                                    processedImageToDeclaration processedFile
                            )
                            processedFiles
                in
                case fromFilesResult of
                    Just fromFiles ->
                        let
                            file : Elm.File
                            file =
                                Elm.expose toPicture.declaration
                                    :: toSources.declaration
                                    :: getSizes_.declaration
                                    :: standardFormats.declaration
                                    :: fromFiles
                                    |> Elm.file [ "Images" ]
                        in
                        Do.allowFatal (BuildTask.writeFile file.contents) <| \hash ->
                        BuildTask.allowFatal (Elm.format tools hash)

                    Nothing ->
                        BuildTask.fail (FatalError.fromString "Failed to process files")
            )


encodeProcessedFiles :
    List
        { svg : Bool
        , filename : Path Path.Relative Path.File
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


processedFileToFileList : ProcessedFile -> List { filename : Path Path.Relative Path.File, hash : FileOrDirectory }
processedFileToFileList file =
    let
        extract : HashedFileWith a -> HashedFileWith {}
        extract original =
            { filename = original.filename
            , hash = original.hash
            }
    in
    case file of
        ProcessedImage imageData ->
            extract imageData.original
                :: List.map extract imageData.converted

        ProcessedCss data ->
            [ extract data ]

        ProcessedSvg data ->
            [ extract data ]

        ProcessedFont data ->
            [ extract data ]


processFile :
    Tools
    -> Inputs
    -> Int
    -> Int
    ->
        ( Path Path.Absolute Path.File
        , BuildTask FatalError FileOrDirectory
        )
    -> BuildTask FatalError (Maybe ProcessedFile)
processFile tools inputs total index ( path, copyFile ) =
    let
        relative : Path Path.Relative Path.File
        relative =
            Path.relativeTo inputs.inputPath path
                |> Path.toString
                |> String.replace " " "_"
                |> Path.parseRelativeFile
                |> trustMe

        prefix : String
        prefix =
            "["
                ++ String.padLeft (String.length (String.fromInt total)) '0' (String.fromInt index)
                ++ "/"
                ++ String.fromInt total
                ++ "]"

        doImage : () -> BuildTask FatalError (Maybe ProcessedFile)
        doImage () =
            BuildTask.do copyFile <| \hash ->
            BuildTask.do (image tools relative hash) <| \data ->
            data
                |> ProcessedImage
                |> Just
                |> BuildTask.succeed

        doSvg : () -> BuildTask FatalError (Maybe ProcessedFile)
        doSvg () =
            BuildTask.do copyFile <| \hash ->
            Do.allowFatal (Image.getSvgSize hash) <| \size ->
            { filename = relative
            , hash = hash
            , width = size.width
            , height = size.height
            }
                |> ProcessedSvg
                |> Just
                |> BuildTask.succeed

        doFont : () -> BuildTask FatalError (Maybe ProcessedFile)
        doFont () =
            BuildTask.do copyFile <| \hash ->
            Do.allowFatal (Font.parse tools hash) <| \fontData ->
            { style = fontData.style
            , weight = fontData.weight
            , family = fontData.family
            , filename = relative
            , hash = hash
            }
                |> ProcessedFont
                |> Just
                |> BuildTask.succeed
    in
    (case Path.fileExtension path of
        Just "webp" ->
            doImage ()

        Just "jpg" ->
            doImage ()

        Just "jpeg" ->
            doImage ()

        Just "png" ->
            doImage ()

        Just "ttf" ->
            doFont ()

        Just "otf" ->
            doFont ()

        Just "svg" ->
            doSvg ()

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


trustMe : Maybe v -> v
trustMe res =
    case res of
        Nothing ->
            let
                _ =
                    -- Crash
                    modBy 0 0
            in
            trustMe res

        Just v ->
            v


{-| Convert an image to jxl, avif, webp, jpg, and scale it down up to 50px.
-}
image :
    Tools
    -> Path Path.Relative Path.File
    -> FileOrDirectory
    ->
        BuildTask
            FatalError
            { original : { width : Int, height : Int, filename : Path Path.Relative Path.File, hash : FileOrDirectory }
            , converted : List (HashedFileWith { width : Int })
            }
image tools relative copied =
    case Path.fileExtension relative of
        Nothing ->
            BuildTask.fail (FatalError.fromString ("Missing extension in " ++ Path.toString relative))

        Just originalExtension ->
            let
                parseSizeFile : String -> BuildTask FatalError { width : Int, height : Int, sizes : List Int }
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
                                    "Could not parse size file: " ++ Utils.escape sizeString
                            in
                            BuildTask.fail (FatalError.fromString msg)

                convertAndResize :
                    FileOrDirectory
                    ->
                        { width : Int
                        , height : Int
                        , sizes : List Int
                        }
                    -> { a | extension : String }
                    -> BuildTask FatalError (List (HashedFileWith { width : Int }))
                convertAndResize stripped sizeData { extension } =
                    convertTo tools extension ( stripped, originalExtension ) <| \converted ->
                    BuildTask.each sizeData.sizes (doResize converted sizeData extension)

                doResize :
                    FileOrDirectory
                    ->
                        { a
                            | width : Int
                            , height : Int
                        }
                    -> String
                    -> Int
                    -> BuildTask FatalError (HashedFileWith { width : Int })
                doResize input sizeData newExtension newWidth =
                    (if newWidth == sizeData.width then
                        BuildTask.succeed input

                     else
                        Unsafe.pipeThrough tools.magick [ "-", "-resize", String.fromInt newWidth ++ "x" ++ String.fromInt sizeData.height, "-" ] input
                            |> BuildTask.allowFatal
                    )
                        |> BuildTask.map
                            (\resized ->
                                { width = newWidth
                                , filename =
                                    Path.splitExtension relative
                                        |> trustMe
                                        |> (\( base, ext ) ->
                                                Path.toString base
                                                    ++ "-"
                                                    ++ String.fromInt newWidth
                                                    ++ "."
                                                    ++ newExtension
                                           )
                                        |> Path.parseRelativeFile
                                        |> trustMe
                                , hash = resized
                                }
                            )
            in
            Do.allowFatal (Image.stripMetadata tools copied) <| \stripped ->
            Do.allowFatal (Image.getSize tools stripped) <| \sizeFile ->
            Do.allowFatal (BuildTask.withFile sizeFile BuildTask.succeed) <| \sizeFileContent ->
            BuildTask.do (parseSizeFile sizeFileContent) <| \sizeData ->
            Do.each standardFormats.list (convertAndResize stripped sizeData) <| \converted ->
            BuildTask.succeed
                { original =
                    { width = sizeData.width
                    , height = sizeData.height
                    , filename = relative
                    , hash = stripped
                    }
                , converted = List.concat converted
                }


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
    Tools
    -> String
    -> ( FileOrDirectory, String )
    -> (FileOrDirectory -> BuildTask FatalError a)
    -> BuildTask FatalError a
convertTo tools format ( cached, originalExtension ) k =
    if originalExtension == format then
        k cached

    else
        BuildTask.allowFatal (Unsafe.pipeThrough tools.magick [ "-", format ++ ":-" ] cached)
            |> BuildTask.andThen k


standardFormats :
    { list : List { format : Elm.Expression, extension : String }
    , declaration : Elm.Declaration
    , value : Elm.Expression
    , internal : Elm.Declare.Internal Elm.Expression
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
    , internal = declaration.internal
    }


processedImageToDeclaration :
    HashedFileWith { a | width : Int, height : Int }
    -> Maybe Elm.Declaration
processedImageToDeclaration original =
    let
        attrsAnnotation : Elm.Annotation.Annotation
        attrsAnnotation =
            Elm.Annotation.list (Gen.Html.annotation_.attribute (Elm.Annotation.var "msg"))
    in
    Maybe.map2
        (\name ( pathWithoutExtension, extension ) ->
            Elm.declaration name
                (Elm.fn
                    (Elm.Arg.varWith "attrs" attrsAnnotation)
                    (\attrs ->
                        toPicture.call attrs
                            (Elm.string
                                (Path.toString
                                    pathWithoutExtension
                                )
                            )
                            (Elm.string extension)
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
        )
        (toVariableName original.filename)
        (Path.splitExtension original.filename)


processedSvgToDeclaration :
    HashedFileWith { a | width : Int, height : Int }
    -> Maybe Elm.Declaration
processedSvgToDeclaration original =
    toVariableName original.filename
        |> Maybe.map
            (\name ->
                let
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
            )


toVariableName : Path Path.Relative Path.File -> Maybe String
toVariableName path =
    Path.splitExtension path
        |> Maybe.map
            (\( nameWithoutExtension, _ ) ->
                let
                    stripLeadingUnderscores i =
                        if String.startsWith "_" i then
                            stripLeadingUnderscores (String.dropLeft 1 i)

                        else
                            i
                in
                nameWithoutExtension
                    |> Path.toString
                    |> String.replace "/" "_"
                    |> String.replace " " "_"
                    |> String.replace "-" "_"
                    |> stripLeadingUnderscores
                    |> String.Extra.decapitalize
            )


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
                  , Elm.Op.Extra.appendStrings
                        [ base
                        , Elm.string "-"
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
