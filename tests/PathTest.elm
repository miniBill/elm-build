module PathTest exposing (suite)

import Expect
import Path.Posix
import Test exposing (Test)


cases :
    List
        { input : String
        , absDir : Maybe String
        , absFile : Maybe String
        , relDir : Maybe String
        , relFile : Maybe String
        }
cases =
    let
        absolutes : List { input : String, absDir : Maybe String, absFile : Maybe String }
        absolutes =
            [ { input = "", absDir = Nothing, absFile = Nothing }

            --
            , { input = "/", absDir = Just "/", absFile = Nothing }
            , { input = "//", absDir = Just "/", absFile = Nothing }
            , { input = "/..", absDir = Just "/", absFile = Nothing }
            , { input = "/../", absDir = Just "/", absFile = Nothing }
            , { input = "/.", absDir = Just "/", absFile = Nothing }
            , { input = "/./", absDir = Just "/", absFile = Nothing }

            --
            , { input = "/a", absDir = Just "/a/", absFile = Just "/a" }
            , { input = "/a/", absDir = Just "/a/", absFile = Nothing }
            ]

        relatives : List { input : String, relDir : Maybe String, relFile : Maybe String }
        relatives =
            [ { input = "..", relDir = Just "../", relFile = Nothing }
            , { input = "../", relDir = Just "../", relFile = Nothing }
            , { input = ".", relDir = Just "./", relFile = Nothing }
            , { input = "./", relDir = Just "./", relFile = Nothing }

            --
            , { input = "a", relDir = Just "a/", relFile = Just "a" }
            , { input = "a/", relDir = Just "a/", relFile = Nothing }

            --
            , { input = "./a", relDir = Just "a/", relFile = Just "a" }
            , { input = "./a/", relDir = Just "a/", relFile = Nothing }
            , { input = "a/.", relDir = Just "a/", relFile = Nothing }
            , { input = "a/./", relDir = Just "a/", relFile = Nothing }
            , { input = "./a/.", relDir = Just "a/", relFile = Nothing }
            , { input = "./a/./", relDir = Just "a/", relFile = Nothing }
            , { input = "../a/..", relDir = Just "../", relFile = Nothing }
            , { input = "../a/../", relDir = Just "../", relFile = Nothing }
            ]
    in
    List.map
        (\i ->
            { input = i.input
            , absDir = i.absDir
            , absFile = i.absFile
            , relDir = Nothing
            , relFile = Nothing
            }
        )
        absolutes
        ++ List.map
            (\i ->
                { input = i.input
                , relDir = i.relDir
                , relFile = i.relFile
                , absDir = Nothing
                , absFile = Nothing
                }
            )
            relatives


suite : Test
suite =
    cases
        |> List.map check
        |> Test.describe "Path.Posix test"


check :
    { input : String
    , absDir : Maybe String
    , absFile : Maybe String
    , relDir : Maybe String
    , relFile : Maybe String
    }
    -> Test
check i =
    Test.describe ("Checking " ++ i.input)
        [ Test.test "parseAbsoluteDirectory" <| \_ ->
        i.input
            |> Path.Posix.parseAbsoluteDirectory
            |> Maybe.map Path.Posix.toString
            |> Expect.equal i.absDir
        , Test.test "parseRelativeDirectory" <| \_ ->
        i.input
            |> Path.Posix.parseRelativeDirectory
            |> Maybe.map Path.Posix.toString
            |> Expect.equal i.relDir
        , Test.test "parseAbsoluteFile" <| \_ ->
        i.input
            |> Path.Posix.parseAbsoluteFile
            |> Maybe.map Path.Posix.toString
            |> Expect.equal i.absFile
        , Test.test "parseRelativeFile" <| \_ ->
        i.input
            |> Path.Posix.parseRelativeFile
            |> Maybe.map Path.Posix.toString
            |> Expect.equal i.relFile
        ]
