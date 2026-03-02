module BuildTask.Do exposing (andThen, each, jobs, map, map2, map3, map4, withFile, writeFile)

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
