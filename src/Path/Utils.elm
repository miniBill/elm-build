module Path.Utils exposing (addTrailingPathSeparator, hasParentDirectory, hasTrailingPathSeparator, isAbsolute, isPathSeparator, isValid, normalise)

import List.Extra


isAbsolute : String -> Bool
isAbsolute p =
    String.startsWith "/" p


isRelative : String -> Bool
isRelative p =
    not (String.startsWith "/" p)


hasTrailingPathSeparator : String -> Bool
hasTrailingPathSeparator p =
    String.endsWith "/" p


isValid : String -> Bool
isValid path =
    not (String.isEmpty path)
        && not (String.contains "\u{0000}" path)


hasParentDirectory : String -> Bool
hasParentDirectory filepath =
    (filepath == "..")
        || String.endsWith "/.." filepath
        || String.contains "/../" filepath
        || String.startsWith "../" filepath


addTrailingPathSeparator : String -> String
addTrailingPathSeparator x =
    if String.endsWith "/" x then
        x

    else
        x ++ "/"


normalise : String -> String
normalise filepath =
    let
        addPathSeparator =
            List.Extra.last pthList
                == Just '/'
                || not (List.isEmpty pthList)
                && (List.Extra.last pthList == Just '.')
                && hasTrailingPathSeparator (String.dropRight 1 pth)
                && not (hasTrailingPathSeparator result)

        ( drv, pthList ) =
            List.Extra.span ((==) '/') (String.toList filepath)

        pth =
            String.fromList pthList

        result =
            joinDrive_
                (if List.isEmpty drv then
                    "/"

                 else
                    String.fromList drv
                )
                (f pth)

        joinDrive_ d p =
            if String.isEmpty d && String.isEmpty p then
                "."

            else
                combineAlways d p

        f p =
            p
                |> splitDirectories
                |> propSep
                |> dropDots
                |> joinPath

        propSep l =
            case l of
                x :: xs ->
                    if String.all ((==) '/') x then
                        "/" :: xs

                    else
                        x :: xs

                [] ->
                    []

        dropDots =
            List.filter ((/=) ".")
    in
    if addPathSeparator then
        result ++ "/"

    else
        result


splitDirectories : String -> List String
splitDirectories p =
    p
        |> splitPath
        |> List.map dropTrailingPathSeparator


splitPath : String -> List String
splitPath x =
    let
        ( drive, path ) =
            List.Extra.span ((==) '/') (String.toList x)

        f y =
            if List.isEmpty y then
                []

            else
                let
                    ( a, b ) =
                        break isPathSeparator y

                    ( c, d ) =
                        span isPathSeparator b
                in
                String.fromList (a ++ c) :: f d
    in
    (if List.isEmpty drive then
        []

     else
        [ String.fromList drive ]
    )
        ++ f path


span : (a -> Bool) -> List a -> ( List a, List a )
span =
    List.Extra.span


break : (a -> Bool) -> List a -> ( List a, List a )
break =
    List.Extra.break


dropTrailingPathSeparator : String -> String
dropTrailingPathSeparator x =
    if
        hasTrailingPathSeparator x
            && not (String.isEmpty (dropDrive x))
    then
        let
            x_ =
                x
                    |> String.toList
                    |> List.Extra.dropWhileRight (\c -> c == '/')
                    |> String.fromList
        in
        if String.isEmpty x_ then
            "/"

        else
            x_

    else
        x


isPathSeparator : Char -> Bool
isPathSeparator c =
    c == '/'


isDrive : String -> Bool
isDrive x =
    not (String.isEmpty x) && String.all ((==) '/') x


dropDrive : String -> String
dropDrive x =
    x
        |> String.toList
        |> List.Extra.dropWhile ((==) '/')
        |> String.fromList


joinPath : List String -> String
joinPath =
    List.foldr combine ""


combine : String -> String -> String
combine a b =
    if String.startsWith "/" b then
        b

    else
        combineAlways a b


hasDrive : String -> Bool
hasDrive s =
    hasLeadingPathSeparator s


takeDrive : String -> String
takeDrive s =
    Tuple.first (splitDrive s)


hasLeadingPathSeparator : String -> Bool
hasLeadingPathSeparator s =
    String.startsWith "/" s


isRelativeDrive : a -> Bool
isRelativeDrive x =
    False


normaliseDrive : String -> String
normaliseDrive bs =
    if String.isEmpty bs then
        ""

    else
        "/"


splitDrive : String -> ( String, String )
splitDrive x =
    List.Extra.span ((==) '/') (String.toList x)
        |> Tuple.mapBoth String.fromList String.fromList


joinDrive : String -> String -> String
joinDrive =
    combineAlways


combineAlways : String -> String -> String
combineAlways a b =
    if String.isEmpty a then
        b

    else if String.isEmpty b then
        a

    else if hasTrailingPathSeparator a then
        a ++ b

    else
        a ++ "/" ++ b
