module BuildTask.Do exposing (andThen, commandWithFile, each, jobs, map, map2, map3, map4, pipeThrough, withFile, writeFile)

import BuildTask exposing (BuildTask, FileOrDirectory)


andThen : (b -> BuildTask a) -> BuildTask b -> BuildTask a
andThen =
    BuildTask.andThen


map :
    (a -> i)
    -> BuildTask a
    -> (i -> BuildTask r)
    -> BuildTask r
map f a k =
    BuildTask.map f a |> andThen k


map2 :
    (a -> b -> i)
    -> BuildTask a
    -> BuildTask b
    -> (i -> BuildTask r)
    -> BuildTask r
map2 f a b k =
    BuildTask.map2 f a b |> andThen k


map3 :
    (a -> b -> c -> i)
    -> BuildTask a
    -> BuildTask b
    -> BuildTask c
    -> (i -> BuildTask r)
    -> BuildTask r
map3 f a b c k =
    BuildTask.map3 f a b c |> andThen k


map4 :
    (a -> b -> c -> d -> i)
    -> BuildTask a
    -> BuildTask b
    -> BuildTask c
    -> BuildTask d
    -> (i -> BuildTask r)
    -> BuildTask r
map4 f a b c d k =
    BuildTask.map4 f a b c d |> andThen k


pipeThrough :
    String
    -> List String
    -> FileOrDirectory
    -> (FileOrDirectory -> BuildTask a)
    -> BuildTask a
pipeThrough cmd args hash k =
    BuildTask.pipeThrough cmd args hash |> andThen k


writeFile :
    String
    -> (FileOrDirectory -> BuildTask a)
    -> BuildTask a
writeFile content k =
    BuildTask.writeFile content |> andThen k


jobs : (Int -> BuildTask a) -> BuildTask a
jobs k =
    BuildTask.jobs |> andThen k


withFile : FileOrDirectory -> (String -> BuildTask b) -> (b -> BuildTask a) -> BuildTask a
withFile hash f k =
    BuildTask.withFile hash f |> andThen k


each : List a -> (a -> BuildTask b) -> (List b -> BuildTask c) -> BuildTask c
each l f k =
    BuildTask.each l f |> andThen k


commandWithFile :
    String
    -> List String
    -> FileOrDirectory
    -> (FileOrDirectory -> BuildTask a)
    -> BuildTask a
commandWithFile cmd args hash k =
    BuildTask.commandWithFile cmd args hash |> andThen k
