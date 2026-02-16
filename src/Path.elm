module Path exposing (Path, appendToFilename, asChildOf, directory, extension, filename, filenameWithoutExtension, path, relativeTo, replaceAll, replaceExtensionWith, toString)

import List.Extra


directory : Path -> Path
directory (Path d _) =
    case List.reverse d of
        [] ->
            Path [] ""

        last :: init ->
            Path (List.reverse init) last


filename : Path -> String
filename (Path _ f) =
    f


filenameWithoutExtension : Path -> String
filenameWithoutExtension (Path _ f) =
    case String.split "." f |> List.reverse of
        [] ->
            -- Should not happen
            ""

        _ :: names ->
            String.join "." (List.reverse names)


path : String -> Path
path p =
    case
        p
            |> String.split "/"
            |> List.reverse
    of
        [] ->
            -- Impossible
            Path [] p

        file :: dir ->
            Path (List.reverse dir) file


type Path
    = Path (List String) String


toString : Path -> String
toString (Path d f) =
    String.join "/" (d ++ [ f ])


asChildOf : String -> Path -> Path
asChildOf newParent (Path d f) =
    Path (newParent :: d) f


relativeTo : Path -> Path -> Path
relativeTo (Path rd _) (Path td tf) =
    let
        _ =
            -- This is a horrible hack
            Debug.todo
    in
    Path (List.drop (List.length rd + 1) td) tf


extension : Path -> Maybe String
extension (Path _ f) =
    f |> String.split "." |> List.Extra.last


replaceExtensionWith : String -> Path -> Path
replaceExtensionWith newExtension (Path d f) =
    case String.split "." f |> List.reverse of
        [] ->
            -- Should not happen
            Path d (f ++ "." ++ newExtension)

        _ :: init ->
            let
                newFilename : List String
                newFilename =
                    List.reverse (newExtension :: init)
            in
            Path d (String.join "." newFilename)


appendToFilename : String -> Path -> Path
appendToFilename toAppend (Path d f) =
    case String.split "." f |> List.reverse of
        last :: name :: init ->
            let
                newFilename : List String
                newFilename =
                    List.reverse (last :: (name ++ toAppend) :: init)
            in
            Path d (String.join "." newFilename)

        [] ->
            Path d (f ++ toAppend)

        [ _ ] ->
            Path d (f ++ toAppend)


replaceAll : String -> String -> Path -> Path
replaceAll from to (Path d f) =
    Path (List.map (\s -> String.replace from to s) d) (String.replace from to f)
