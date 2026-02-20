module DepGraph exposing
    ( Graph
    , parseImports
    , filePathToModuleName
    , buildGraph
    , transitiveDeps
    )

{-| Pure dependency graph analysis for Elm source files.

Parses import statements and builds a transitive dependency graph
so each test can be cached based only on files it actually depends on.

-}

import Dict exposing (Dict)
import Set exposing (Set)


{-| An opaque dependency graph. Maps file paths to the set of file paths they directly depend on.
-}
type Graph
    = Graph
        { deps : Dict String (Set String)
        }


{-| Parse import statements from Elm source code.

Only lines starting with exactly "import " at column 0 are recognized.
Comment lines and indented lines are ignored.
Returns module names as strings (e.g., "Foo.Bar.Baz").

-}
parseImports : String -> List String
parseImports source =
    source
        |> String.lines
        |> List.filterMap parseImportLine


parseImportLine : String -> Maybe String
parseImportLine line =
    if String.startsWith "import " line then
        let
            rest =
                String.dropLeft 7 line |> String.trimLeft
        in
        -- The module name is everything up to the first space (or end of line)
        case String.words rest of
            moduleName :: _ ->
                Just moduleName

            [] ->
                Nothing

    else
        Nothing


{-| Convert a file path to a module name given source directories.

    filePathToModuleName [ "src" ] "src/Foo/Bar.elm" == Just "Foo.Bar"
    filePathToModuleName [ "src" ] "tests/Foo.elm" == Nothing

Trailing slashes on source directories are normalized.
Only `.elm` files are recognized.

-}
filePathToModuleName : List String -> String -> Maybe String
filePathToModuleName sourceDirectories filePath =
    if not (String.endsWith ".elm" filePath) then
        Nothing

    else
        sourceDirectories
            |> List.filterMap
                (\dir ->
                    let
                        normalizedDir =
                            if String.endsWith "/" dir then
                                dir

                            else
                                dir ++ "/"
                    in
                    if String.startsWith normalizedDir filePath then
                        filePath
                            |> String.dropLeft (String.length normalizedDir)
                            |> String.dropRight 4
                            |> String.replace "/" "."
                            |> Just

                    else
                        Nothing
                )
            |> List.head


{-| Build a dependency graph from source directories and file contents.

External/package imports (modules not found in the provided files) are silently dropped.

-}
buildGraph : { sourceDirectories : List String, files : List { filePath : String, content : String } } -> Graph
buildGraph { sourceDirectories, files } =
    let
        -- Map module name -> file path
        moduleToFile : Dict String String
        moduleToFile =
            files
                |> List.filterMap
                    (\{ filePath } ->
                        filePathToModuleName sourceDirectories filePath
                            |> Maybe.map (\moduleName -> ( moduleName, filePath ))
                    )
                |> Dict.fromList

        -- For each file, find its direct dependencies (as file paths)
        deps : Dict String (Set String)
        deps =
            files
                |> List.map
                    (\{ filePath, content } ->
                        let
                            imports =
                                parseImports content

                            depFilePaths =
                                imports
                                    |> List.filterMap (\moduleName -> Dict.get moduleName moduleToFile)
                                    |> Set.fromList
                        in
                        ( filePath, depFilePaths )
                    )
                |> Dict.fromList
    in
    Graph { deps = deps }


{-| Get all transitive dependencies of a file (including itself).

Uses BFS with a visited set to handle circular dependencies.
Returns file paths. If the file path is unknown, returns a singleton set.

-}
transitiveDeps : Graph -> String -> Set String
transitiveDeps (Graph { deps }) startFile =
    bfs deps [ startFile ] (Set.singleton startFile)


bfs : Dict String (Set String) -> List String -> Set String -> Set String
bfs deps queue visited =
    case queue of
        [] ->
            visited

        current :: rest ->
            let
                directDeps =
                    Dict.get current deps
                        |> Maybe.withDefault Set.empty

                newDeps =
                    Set.diff directDeps visited

                newQueue =
                    rest ++ Set.toList newDeps

                newVisited =
                    Set.union visited newDeps
            in
            bfs deps newQueue newVisited
