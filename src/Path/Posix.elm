module Path.Posix exposing
    ( Path
    , Absolute, Relative, AbsoluteOrRelative
    , File, Directory, FileOrDirectory
    , AbsoluteOrRelativePath(..)
    , append, parent, filename, dirname, splitExtension, fileExtension, replaceExtension, mapAbsoluteOrRelative, extractAbsoluteOrRelative, isPrefixOf
    , parseAbsoluteDirectory, parseRelativeDirectory, parseAbsoluteFile, parseRelativeFile, parseRelativeFileOrDirectory
    , toString, absoluteDirectoryToString, relativeDirectoryToString, absoluteFileToString, relativeFileToString
    , relativeTo, splitDirectory, toFileOrDirectory
    )

{-| Module for handling paths. The API is inspired by [path](https://hackage.haskell.org/package/path).


## Types

@docs Path
@docs Absolute, Relative, AbsoluteOrRelative
@docs File, Directory, FileOrDirectory
@docs AbsoluteOrRelativePath


## Operations

@docs append, replaceProperPrefix, parent, filename, dirname, splitExtension, fileExtension, replaceExtension, mapAbsoluteOrRelative, extractAbsoluteOrRelative, isPrefixOf


## Parsing

@docs parseAbsoluteDirectory, parseRelativeDirectory, parseAbsoluteFile, parseRelativeFile, parseAbsoluteFileOrDirectory, parseRelativeFileOrDirectory


## Conversion

@docs toString, absoluteDirectoryToString, relativeDirectoryToString, absoluteFileToString, relativeFileToString


## Utilities

@docs relativeTo, splitDirectory, toFileOrDirectory


## License notice

Some of the code and documentation are derived from the `path` Haskell library, licensed under BSD-3-Clause:

Copyright (c) 2015–2018, FP Complete
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
  - Neither the name of paths nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

import List.Extra


{-| Path of some base and type.

The type variables are:

  - `base` - the base location of the path; absolute or relative;
  - `kind` - whether file or directory.

Internally is a list of strings. The string can be of two formats only:

1.  file format: file.txt, foo/bar.txt, /foo/bar.txt;
2.  directory format: foo/, /foo/bar/.

All directories end in a trailing separator. There are no duplicate path separators //, no ./, no ~/, etc.

-}
type
    Path base kind
    -- A directory will always end with ""
    -- `/` is [ "", "" ]
    -- `.` is [ "" ]
    = Path (List String)


{-| An absolute path.
-}
type Absolute
    = Absolute_


{-| A relative path; one without a root.
-}
type Relative
    = Relative_


{-| A file path.
-}
type File
    = File_


{-| A relative or absolute path.
-}
type AbsoluteOrRelative
    = AbsoluteOrRelative_


{-| A directory path.
-}
type Directory
    = Directory_


{-| A path which could either be a file or a directory.
-}
type FileOrDirectory
    = FileOrDirectory_


{-| Path of some type. `kind` represents the type, whether file or directory. Pattern match to find whether the path is absolute or relative.
-}
type AbsoluteOrRelativePath kind
    = Absolute (Path Absolute kind)
    | Relative (Path Relative kind)


{-| Append two paths.
-}
append : Path base Directory -> Path Relative kind -> Path base kind
append (Path l) (Path r) =
    (l ++ r)
        |> normalize
        |> Path


{-| -}
toString : Path base kind -> String
toString (Path p) =
    case p of
        [ "" ] ->
            "./"

        o ->
            String.join "/" o


{-| Specialized version of `toString`. You can use it to make code more explicit and add further compile-time checks.
-}
absoluteDirectoryToString : Path Absolute Directory -> String
absoluteDirectoryToString p =
    toString p


{-| Specialized version of `toString`. You can use it to make code more explicit and add further compile-time checks.
-}
relativeDirectoryToString : Path Relative Directory -> String
relativeDirectoryToString p =
    toString p


{-| Specialized version of `toString`. You can use it to make code more explicit and add further compile-time checks.
-}
absoluteFileToString : Path Absolute File -> String
absoluteFileToString p =
    toString p


{-| Specialized version of `toString`. You can use it to make code more explicit and add further compile-time checks.
-}
relativeFileToString : Path Relative File -> String
relativeFileToString p =
    toString p


parseAbsoluteDirectory : String -> Maybe (Path Absolute Directory)
parseAbsoluteDirectory filepath =
    if String.startsWith "/" filepath && isValid filepath then
        (splitAndNormalize filepath ++ [ "" ])
            |> Path
            |> Just

    else
        Nothing


parseRelativeDirectory : String -> Maybe (Path Relative Directory)
parseRelativeDirectory filepath =
    if not (String.startsWith "/" filepath) && isValid filepath then
        (splitAndNormalize filepath ++ [ "" ])
            |> Path
            |> Just

    else
        Nothing


isValid : String -> Bool
isValid filepath =
    not (String.isEmpty filepath) && not (String.contains "\u{0000}" filepath)


parseAbsoluteFile : String -> Maybe (Path Absolute File)
parseAbsoluteFile filepath =
    if
        String.startsWith "/" filepath
            && not (String.endsWith "/" filepath)
            && not (String.endsWith "/." filepath)
            && not (String.endsWith "/.." filepath)
            && isValid filepath
    then
        filepath
            |> splitAndNormalize
            |> Path
            |> Just

    else
        Nothing


{-| Convert a relative path to a normalized relative file `Path`.

Returns `Nothing` when the supplied path:

  - is not a relative path

  - is `""`

  - is a directory path i.e.
      - has a trailing path separator
      - is `.` or ends in `/.`
      - is `..` or ends in `/..`

  - is not a valid path (See 'FilePath.isValid')

-}
parseRelativeFile : String -> Maybe (Path Relative File)
parseRelativeFile filepath =
    if
        not (String.startsWith "/" filepath)
            && (filepath /= ".")
            && (filepath /= "..")
            && not (String.endsWith "/" filepath)
            && not (String.endsWith "/." filepath)
            && not (String.endsWith "/.." filepath)
            && isValid filepath
    then
        case splitAndNormalize filepath of
            [ "" ] ->
                Nothing

            normalized ->
                case List.Extra.last normalized of
                    Just "." ->
                        Nothing

                    Just ".." ->
                        Nothing

                    _ ->
                        Just (Path normalized)

    else
        Nothing


parseRelativeFileOrDirectory : String -> Maybe (Path Relative FileOrDirectory)
parseRelativeFileOrDirectory path =
    case parseRelativeFile path of
        Just (Path p) ->
            Just (Path p)

        Nothing ->
            case parseRelativeDirectory path of
                Just (Path p) ->
                    Just (Path p)

                Nothing ->
                    Nothing


parent : Path base kind -> Path base Directory
parent (Path p) =
    case p of
        [ "" ] ->
            -- `.`
            Path [ ".." ]

        [ "", "" ] ->
            -- `/`
            Path p

        _ ->
            normalize (p ++ [ ".." ])
                |> Path


filename : Path base File -> Path Relative File
filename (Path p) =
    case List.Extra.last p of
        Nothing ->
            -- Impossible
            Path []

        Just name ->
            Path [ name ]


dirname : Path base Directory -> Path Relative Directory
dirname (Path p) =
    case List.Extra.last (List.Extra.removeWhen String.isEmpty p) of
        Nothing ->
            -- /
            Path [ "", "" ]

        Just name ->
            Path [ name ]


fileExtension : Path base File -> Maybe String
fileExtension (Path p) =
    case List.Extra.last p of
        Nothing ->
            -- Impossible
            Nothing

        Just name ->
            name
                |> String.split "."
                |> List.reverse
                |> List.take 1
                |> String.concat
                |> Just


replaceExtension : String -> Path base File -> Maybe (Path base File)
replaceExtension ext (Path p) =
    case List.reverse p of
        [] ->
            -- Impossible
            Nothing

        file :: reversePath ->
            let
                replaced : String
                replaced =
                    file
                        |> String.split "."
                        |> List.reverse
                        |> List.drop 1
                        |> (::) ext
                        |> List.reverse
                        |> String.join "."
            in
            (replaced :: reversePath)
                |> List.reverse
                |> Path
                |> Just


splitExtension : Path base File -> Maybe ( Path base File, String )
splitExtension (Path p) =
    case List.reverse p of
        [] ->
            -- Impossible
            Nothing

        file :: reversePath ->
            let
                ( reverseFilename, ext ) =
                    file
                        |> String.split "."
                        |> List.reverse
                        |> List.Extra.splitAt 1

                replaced : String
                replaced =
                    reverseFilename
                        |> List.reverse
                        |> String.join "."
            in
            Just
                ( Path (List.reverse (replaced :: reversePath))
                , String.concat ext
                )


{-| Helper to apply a function to an `AbsoluteOrRelativePath` value.
-}
mapAbsoluteOrRelative :
    (Path Absolute kind -> Path Absolute kind2)
    -> (Path Relative kind -> Path Relative kind2)
    -> AbsoluteOrRelativePath kind
    -> AbsoluteOrRelativePath kind2
mapAbsoluteOrRelative onAbs onRel path =
    case path of
        Absolute abs ->
            Absolute (onAbs abs)

        Relative rel ->
            Relative (onRel rel)


{-| Helper to extract the contents out of an `AbsoluteOrRelativePath` value.
-}
extractAbsoluteOrRelative :
    (Path Absolute kind -> a)
    -> (Path Relative kind -> a)
    -> AbsoluteOrRelativePath kind
    -> a
extractAbsoluteOrRelative onAbs onRel path =
    case path of
        Absolute abs ->
            onAbs abs

        Relative rel ->
            onRel rel


splitDirectory : Path Relative Directory -> List (Path Relative Directory)
splitDirectory (Path p) =
    p
        |> List.filterMap
            (\s ->
                if String.isEmpty s then
                    Nothing

                else
                    Just (Path [ s, "" ])
            )


toFileOrDirectory : Path base kind -> Path base FileOrDirectory
toFileOrDirectory (Path p) =
    Path p


isPrefixOf : Path base kind -> Path base Directory -> Bool
isPrefixOf (Path child) (Path base) =
    List.Extra.isPrefixOf child (base |> List.Extra.dropWhileRight String.isEmpty)


relativeTo : Path base Directory -> Path base fileOrDirectory -> Path Relative fileOrDirectory
relativeTo (Path base) (Path fileOrDirectory) =
    case ( base, fileOrDirectory ) of
        ( [], _ ) ->
            Debug.todo ("Path.relativeTo " ++ toString (Path base) ++ " " ++ toString (Path fileOrDirectory))

        ( _, [] ) ->
            Debug.todo ("Path.relativeTo " ++ toString (Path base) ++ " " ++ toString (Path fileOrDirectory))

        ( [ "" ], _ ) ->
            Path fileOrDirectory

        ( baseHead :: baseTail, fileHead :: fileTail ) ->
            if baseHead == fileHead then
                relativeTo (Path baseTail) (Path fileTail)

            else
                Debug.todo ("Path.relativeTo " ++ toString (Path base) ++ " " ++ toString (Path fileOrDirectory))


splitAndNormalize : String -> List String
splitAndNormalize filepath =
    String.split "/" filepath
        |> normalize


normalize : List String -> List String
normalize splat =
    splat
        |> List.foldl
            (\segment ( isFirst, acc ) ->
                case segment of
                    "" ->
                        ( False
                        , if isFirst then
                            [ "" ]

                          else
                            acc
                        )

                    "." ->
                        ( False, acc )

                    ".." ->
                        ( False
                        , case acc of
                            [ "" ] ->
                                -- /..
                                [ "" ]

                            [] ->
                                -- ../
                                [ segment ]

                            ".." :: _ ->
                                segment :: acc

                            _ :: tail ->
                                tail
                        )

                    _ ->
                        ( False, segment :: acc )
            )
            ( True, [] )
        |> Tuple.second
        |> List.reverse
