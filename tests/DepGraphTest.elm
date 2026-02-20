module DepGraphTest exposing (suite)

import DepGraph
import Expect
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "DepGraph"
        [ parseImportsBasics
        , parseImportsEdgeCases
        , filePathToModuleNameTests
        , buildGraphAndTransitiveDeps
        , integrationTest
        ]



-- PHASE 1: parseImports basics


parseImportsBasics : Test
parseImportsBasics =
    describe "parseImports basics"
        [ test "1. empty file → []" <|
            \_ ->
                DepGraph.parseImports ""
                    |> Expect.equal []
        , test "2. single import → [\"Bar\"]" <|
            \_ ->
                DepGraph.parseImports "module Foo exposing (..)\n\nimport Bar\n"
                    |> Expect.equal [ "Bar" ]
        , test "3. multiple imports → [\"Bar\", \"Baz\", \"Qux\"]" <|
            \_ ->
                DepGraph.parseImports "module Foo exposing (..)\n\nimport Bar\nimport Baz\nimport Qux\n"
                    |> Expect.equal [ "Bar", "Baz", "Qux" ]
        , test "4. import with exposing → [\"Dict\"]" <|
            \_ ->
                DepGraph.parseImports "import Dict exposing (Dict)"
                    |> Expect.equal [ "Dict" ]
        , test "5. import with as → [\"Json.Decode\"]" <|
            \_ ->
                DepGraph.parseImports "import Json.Decode as Decode"
                    |> Expect.equal [ "Json.Decode" ]
        , test "6. import with as + exposing → [\"Html.Attributes\"]" <|
            \_ ->
                DepGraph.parseImports "import Html.Attributes as Attr exposing (class, id)"
                    |> Expect.equal [ "Html.Attributes" ]
        , test "7. dotted module names → [\"Foo.Bar.Baz.Qux\"]" <|
            \_ ->
                DepGraph.parseImports "import Foo.Bar.Baz.Qux"
                    |> Expect.equal [ "Foo.Bar.Baz.Qux" ]
        ]



-- PHASE 2: parseImports edge cases


parseImportsEdgeCases : Test
parseImportsEdgeCases =
    describe "parseImports edge cases"
        [ test "8. comment lines (-- import Foo) ignored" <|
            \_ ->
                DepGraph.parseImports "-- import Foo\nimport Bar\n"
                    |> Expect.equal [ "Bar" ]
        , test "9. indented lines (    import Foo) ignored" <|
            \_ ->
                DepGraph.parseImports "    import Foo\nimport Bar\n"
                    |> Expect.equal [ "Bar" ]
        , test "10. duplicate imports preserved" <|
            \_ ->
                DepGraph.parseImports "import Foo\nimport Foo\n"
                    |> Expect.equal [ "Foo", "Foo" ]
        ]



-- PHASE 3: filePathToModuleName


filePathToModuleNameTests : Test
filePathToModuleNameTests =
    describe "filePathToModuleName"
        [ test "11. [\"src\"] \"src/Foo.elm\" → Just \"Foo\"" <|
            \_ ->
                DepGraph.filePathToModuleName [ "src" ] "src/Foo.elm"
                    |> Expect.equal (Just "Foo")
        , test "12. [\"src\"] \"src/Foo/Bar/Baz.elm\" → Just \"Foo.Bar.Baz\"" <|
            \_ ->
                DepGraph.filePathToModuleName [ "src" ] "src/Foo/Bar/Baz.elm"
                    |> Expect.equal (Just "Foo.Bar.Baz")
        , test "13. [\"src\", \"codegen\"] \"codegen/Gen/Html.elm\" → Just \"Gen.Html\"" <|
            \_ ->
                DepGraph.filePathToModuleName [ "src", "codegen" ] "codegen/Gen/Html.elm"
                    |> Expect.equal (Just "Gen.Html")
        , test "14. [\"src\"] \"tests/MyTest.elm\" → Nothing" <|
            \_ ->
                DepGraph.filePathToModuleName [ "src" ] "tests/MyTest.elm"
                    |> Expect.equal Nothing
        , test "15. [\"src\"] \"src/Foo.js\" → Nothing" <|
            \_ ->
                DepGraph.filePathToModuleName [ "src" ] "src/Foo.js"
                    |> Expect.equal Nothing
        , test "16. trailing slash normalization: [\"src/\"] \"src/Foo.elm\" → Just \"Foo\"" <|
            \_ ->
                DepGraph.filePathToModuleName [ "src/" ] "src/Foo.elm"
                    |> Expect.equal (Just "Foo")
        ]



-- PHASE 4: buildGraph + transitiveDeps


buildGraphAndTransitiveDeps : Test
buildGraphAndTransitiveDeps =
    describe "buildGraph + transitiveDeps"
        [ test "17. single file, no imports → Set.singleton \"src/Main.elm\"" <|
            \_ ->
                let
                    graph =
                        DepGraph.buildGraph
                            { sourceDirectories = [ "src" ]
                            , files =
                                [ { filePath = "src/Main.elm"
                                  , content = "module Main exposing (..)\n"
                                  }
                                ]
                            }
                in
                DepGraph.transitiveDeps graph "src/Main.elm"
                    |> Expect.equal (Set.singleton "src/Main.elm")
        , test "18. two files, one import → direct dep included" <|
            \_ ->
                let
                    graph =
                        DepGraph.buildGraph
                            { sourceDirectories = [ "src" ]
                            , files =
                                [ { filePath = "src/Main.elm"
                                  , content = "module Main exposing (..)\n\nimport Helper\n"
                                  }
                                , { filePath = "src/Helper.elm"
                                  , content = "module Helper exposing (..)\n"
                                  }
                                ]
                            }
                in
                DepGraph.transitiveDeps graph "src/Main.elm"
                    |> Expect.equal (Set.fromList [ "src/Main.elm", "src/Helper.elm" ])
        , test "19. external imports (Dict, Set) silently dropped" <|
            \_ ->
                let
                    graph =
                        DepGraph.buildGraph
                            { sourceDirectories = [ "src" ]
                            , files =
                                [ { filePath = "src/Main.elm"
                                  , content = "module Main exposing (..)\n\nimport Dict\nimport Set\nimport Helper\n"
                                  }
                                , { filePath = "src/Helper.elm"
                                  , content = "module Helper exposing (..)\n"
                                  }
                                ]
                            }
                in
                DepGraph.transitiveDeps graph "src/Main.elm"
                    |> Expect.equal (Set.fromList [ "src/Main.elm", "src/Helper.elm" ])
        , test "20. chain A→B→C → all three in set" <|
            \_ ->
                let
                    graph =
                        DepGraph.buildGraph
                            { sourceDirectories = [ "src" ]
                            , files =
                                [ { filePath = "src/A.elm"
                                  , content = "module A exposing (..)\n\nimport B\n"
                                  }
                                , { filePath = "src/B.elm"
                                  , content = "module B exposing (..)\n\nimport C\n"
                                  }
                                , { filePath = "src/C.elm"
                                  , content = "module C exposing (..)\n"
                                  }
                                ]
                            }
                in
                DepGraph.transitiveDeps graph "src/A.elm"
                    |> Expect.equal (Set.fromList [ "src/A.elm", "src/B.elm", "src/C.elm" ])
        , test "21. diamond A→B,C; B,C→D → all four in set" <|
            \_ ->
                let
                    graph =
                        DepGraph.buildGraph
                            { sourceDirectories = [ "src" ]
                            , files =
                                [ { filePath = "src/A.elm"
                                  , content = "module A exposing (..)\n\nimport B\nimport C\n"
                                  }
                                , { filePath = "src/B.elm"
                                  , content = "module B exposing (..)\n\nimport D\n"
                                  }
                                , { filePath = "src/C.elm"
                                  , content = "module C exposing (..)\n\nimport D\n"
                                  }
                                , { filePath = "src/D.elm"
                                  , content = "module D exposing (..)\n"
                                  }
                                ]
                            }
                in
                DepGraph.transitiveDeps graph "src/A.elm"
                    |> Expect.equal (Set.fromList [ "src/A.elm", "src/B.elm", "src/C.elm", "src/D.elm" ])
        , test "22. circular A→B→A → both in set (no infinite loop)" <|
            \_ ->
                let
                    graph =
                        DepGraph.buildGraph
                            { sourceDirectories = [ "src" ]
                            , files =
                                [ { filePath = "src/A.elm"
                                  , content = "module A exposing (..)\n\nimport B\n"
                                  }
                                , { filePath = "src/B.elm"
                                  , content = "module B exposing (..)\n\nimport A\n"
                                  }
                                ]
                            }
                in
                DepGraph.transitiveDeps graph "src/A.elm"
                    |> Expect.equal (Set.fromList [ "src/A.elm", "src/B.elm" ])
        , test "23. unknown file path → singleton of that path" <|
            \_ ->
                let
                    graph =
                        DepGraph.buildGraph
                            { sourceDirectories = [ "src" ]
                            , files = []
                            }
                in
                DepGraph.transitiveDeps graph "src/Unknown.elm"
                    |> Expect.equal (Set.singleton "src/Unknown.elm")
        , test "24. file with only external imports → singleton" <|
            \_ ->
                let
                    graph =
                        DepGraph.buildGraph
                            { sourceDirectories = [ "src" ]
                            , files =
                                [ { filePath = "src/Main.elm"
                                  , content = "module Main exposing (..)\n\nimport Dict\nimport Set\nimport Json.Decode\n"
                                  }
                                ]
                            }
                in
                DepGraph.transitiveDeps graph "src/Main.elm"
                    |> Expect.equal (Set.singleton "src/Main.elm")
        ]



-- PHASE 5: Integration test


integrationTest : Test
integrationTest =
    describe "integration"
        [ test "25. multi-source-directory real-world scenario" <|
            \_ ->
                let
                    graph =
                        DepGraph.buildGraph
                            { sourceDirectories = [ "src", "codegen" ]
                            , files =
                                [ { filePath = "src/App.elm"
                                  , content = "module App exposing (..)\n\nimport Route\nimport Gen.Html\nimport Dict\n"
                                  }
                                , { filePath = "src/Route.elm"
                                  , content = "module Route exposing (..)\n\nimport Utils\n"
                                  }
                                , { filePath = "src/Utils.elm"
                                  , content = "module Utils exposing (..)\n"
                                  }
                                , { filePath = "codegen/Gen/Html.elm"
                                  , content = "module Gen.Html exposing (..)\n\nimport Gen.Internal\n"
                                  }
                                , { filePath = "codegen/Gen/Internal.elm"
                                  , content = "module Gen.Internal exposing (..)\n"
                                  }
                                , { filePath = "src/Unrelated.elm"
                                  , content = "module Unrelated exposing (..)\n\nimport Dict\n"
                                  }
                                ]
                            }

                    appDeps =
                        DepGraph.transitiveDeps graph "src/App.elm"

                    unrelatedDeps =
                        DepGraph.transitiveDeps graph "src/Unrelated.elm"
                in
                Expect.all
                    [ \_ ->
                        appDeps
                            |> Expect.equal
                                (Set.fromList
                                    [ "src/App.elm"
                                    , "src/Route.elm"
                                    , "src/Utils.elm"
                                    , "codegen/Gen/Html.elm"
                                    , "codegen/Gen/Internal.elm"
                                    ]
                                )
                    , \_ ->
                        unrelatedDeps
                            |> Expect.equal (Set.singleton "src/Unrelated.elm")
                    , \_ ->
                        -- Unrelated.elm should NOT be in App's deps
                        Set.member "src/Unrelated.elm" appDeps
                            |> Expect.equal False
                    ]
                    ()
        ]
