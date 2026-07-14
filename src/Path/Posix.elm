module Path.Posix exposing
    ( Path
    , Absolute, Relative, AbsoluteOrRelative
    , File, Directory, FileOrDirectory
    , AbsoluteOrRelativePath(..)
    , append, stripProperPrefix, isProperPrefixOf, replaceProperPrefix, parent, filename, dirname, addExtension, splitExtension, fileExtension, replaceExtension, mapAbsoluteOrRelative, extractAbsoluteOrRelative
    , parseAbsoluteDirectory, parseRelativeDirectory, parseAbsoluteFile, parseRelativeFile, parseAbsoluteFileOrDirectory, parseRelativeFileOrDirectory
    , toString, absoluteDirectoryToString, relativeDirectoryToString, absoluteFileToString, relativeFileToString
    , AbsoluteError, ParseError, RelativeError, relativeTo, splitDirectory, toFileOrDirectory
    )

{-| Module for handling paths. The implementation is based on [path](https://hackage.haskell.org/package/path).


## Types

@docs Path
@docs Absolute, Relative, AbsoluteOrRelative
@docs File, Directory, FileOrDirectory
@docs AbsoluteOrRelativePath


## Operations

@docs append, stripProperPrefix, isProperPrefixOf, replaceProperPrefix, parent, filename, dirname, addExtension, splitExtension, fileExtension, replaceExtension, mapAbsoluteOrRelative, extractAbsoluteOrRelative


## Parsing

@docs parseAbsoluteDirectory, parseRelativeDirectory, parseAbsoluteFile, parseRelativeFile, parseAbsoluteFileOrDirectory, parseRelativeFileOrDirectory


## Conversion

@docs toString, absoluteDirectoryToString, relativeDirectoryToString, absoluteFileToString, relativeFileToString


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

import Result.Extra


{-| Path of some base and type.

The type variables are:

  - `base` - the base location of the path; absolute or relative;
  - `kind` - whether file or directory.

Internally is a string. The string can be of two formats only:

1.  file format: file.txt, foo/bar.txt, /foo/bar.txt;
2.  directory format: foo/, /foo/bar/.

All directories end in a trailing separator. There are no duplicate path separators //, no .., no ./, no ~/, etc.

To handle `..` and `~` you can use `resolveFile` / `resolveDirectory`.

-}
type Path base kind
    = Path String


{-| An absolute path.
-}
type Absolute
    = Absolute_


{-| A relative path; one without a root. Note that a `..` path component to represent the parent directory is not allowed by this library.
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
    Path (l ++ r)


type StripProperPrefixError
    = NotAProperPrefix


stripProperPrefix : Path base Directory -> Path base kind -> Result StripProperPrefixError (Path Relative kind)
stripProperPrefix (Path base) (Path child) =
    if String.startsWith base child then
        let
            stripped =
                String.dropLeft (String.length base) child
        in
        if String.isEmpty stripped then
            Err NotAProperPrefix

        else
            Ok (Path stripped)

    else
        Err NotAProperPrefix


isProperPrefixOf : Path base Directory -> Path base kind -> Bool
isProperPrefixOf p l =
    Result.Extra.isOk (stripProperPrefix p l)


replaceProperPrefix :
    Path prefixedBase Directory
    -> Path base Directory
    -> Path prefixedBase kind
    -> Result StripProperPrefixError (Path base kind)
replaceProperPrefix src dst fp =
    Result.map (append dst) (stripProperPrefix src fp)


type AddExtensionError
    = InvalidExtension


addExtension : String -> Path base File -> Result AddExtensionError (Path base File)
addExtension ext (Path path) =
    case String.toList ext of
        '.' :: xs ->
            if List.isEmpty xs || List.member '/' xs || List.all ((==) '.') xs then
                Err InvalidExtension

            else
                case parseRelativeFile ext of
                    Err _ ->
                        Err InvalidExtension

                    Ok _ ->
                        Ok (Path (path ++ ext))

        _ ->
            Err InvalidExtension


{-| -}
toString : Path base kind -> String
toString (Path p) =
    p


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


{-| The errors that can be encountered while parsing a path.

  - `EmptyInput` - the input was empty
  - `ContainsDotDot` - the input contains `..` - if your inputs can contain `..` you should use `resolveFile` / `resolveDirectory` instead
  - `BaseError` - errors specific to `Relative` / `Absolute` parsing

-}
type ParseError base
    = EmptyInput
    | ContainsDotDot
    | BaseError base


{-| The path does not start with `/` even though it should have been absolute.
-}
type AbsoluteError
    = DoesNotStartWithSlash


{-| The path starts with `/` even though it should have been relative.
-}
type RelativeError
    = StartsWithSlash


parseAbsoluteDirectory : String -> Result (ParseError AbsoluteError) (Path Absolute Directory)
parseAbsoluteDirectory p =
    Debug.todo "Path.parseAbsoluteDirectory"


parseRelativeDirectory : String -> Result (ParseError RelativeError) (Path Relative Directory)
parseRelativeDirectory p =
    Debug.todo "Path.parseRelativeDirectory"


parseAbsoluteFile : String -> Result (ParseError AbsoluteError) (Path Absolute File)
parseAbsoluteFile p =
    Debug.todo "Path.parseAbsoluteFile"


parseRelativeFile : String -> Result (ParseError RelativeError) (Path Relative File)
parseRelativeFile p =
    Debug.todo "Path.parseRelativeFile"


parseAbsoluteFileOrDirectory : String -> Result (ParseError AbsoluteError) (Path Absolute FileOrDirectory)
parseAbsoluteFileOrDirectory p =
    Debug.todo "Path.parseAbsoluteFileOrDirectory"


parseRelativeFileOrDirectory : String -> Result (ParseError RelativeError) (Path Relative FileOrDirectory)
parseRelativeFileOrDirectory p =
    Debug.todo "Path.parseRelativeFileOrDirectory"


parent : Path base kind -> Path base Directory
parent (Path p) =
    Debug.todo "Path.parent"


filename : Path base File -> Path Relative File
filename (Path p) =
    Debug.todo "Path.filename"


dirname : Path base Directory -> Path Relative Directory
dirname (Path p) =
    Debug.todo "Path.dirname"


fileExtension : Path base File -> Result () String
fileExtension (Path p) =
    Debug.todo "Path.fileExtension"


replaceExtension : String -> Path base File -> Result () (Path base File)
replaceExtension ext (Path p) =
    Debug.todo "Path.replaceExtension"


splitExtension : Path base File -> Result () ( Path base File, String )
splitExtension (Path p) =
    Debug.todo "Path.splitExtension"


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
splitDirectory p =
    splitDirectoryHelp p []


splitDirectoryHelp : Path Relative Directory -> List (Path Relative Directory) -> List (Path Relative Directory)
splitDirectoryHelp p acc =
    let
        next : Path Relative Directory
        next =
            parent p
    in
    if next == p then
        List.reverse acc

    else
        splitDirectoryHelp next (dirname p :: acc)


toFileOrDirectory : Path base kind -> Path base FileOrDirectory
toFileOrDirectory (Path p) =
    Path p


relativeTo : Path base Directory -> Path base2 fileOrDirectory -> Path Relative fileOrDirectory
relativeTo base fileOrDirectory =
    Debug.todo ("Path.relativeTo " ++ toString base ++ " " ++ toString fileOrDirectory)
