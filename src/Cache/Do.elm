module Cache.Do exposing (..)

import Cache exposing (FileOrDirectory, Monad)


andThen : (b -> Monad a) -> Monad b -> Monad a
andThen =
    Cache.andThen


map :
    (a -> i)
    -> Monad a
    -> (i -> Monad r)
    -> Monad r
map f a k =
    Cache.map f a |> andThen k


map2 :
    (a -> b -> i)
    -> Monad a
    -> Monad b
    -> (i -> Monad r)
    -> Monad r
map2 f a b k =
    Cache.map2 f a b |> andThen k


map3 :
    (a -> b -> c -> i)
    -> Monad a
    -> Monad b
    -> Monad c
    -> (i -> Monad r)
    -> Monad r
map3 f a b c k =
    Cache.map3 f a b c |> andThen k


map4 :
    (a -> b -> c -> d -> i)
    -> Monad a
    -> Monad b
    -> Monad c
    -> Monad d
    -> (i -> Monad r)
    -> Monad r
map4 f a b c d k =
    Cache.map4 f a b c d |> andThen k


pipeThrough :
    String
    -> List String
    -> FileOrDirectory
    -> (FileOrDirectory -> Monad a)
    -> Monad a
pipeThrough cmd args hash k =
    Cache.pipeThrough cmd args hash |> andThen k


writeFile :
    String
    -> (FileOrDirectory -> Monad a)
    -> Monad a
writeFile content k =
    Cache.writeFile content |> andThen k


jobs : (Int -> Monad a) -> Monad a
jobs k =
    Cache.jobs |> andThen k


withFile : FileOrDirectory -> (String -> Monad b) -> (b -> Monad a) -> Monad a
withFile hash f k =
    Cache.withFile hash f |> andThen k


each : List a -> (a -> Monad b) -> (List b -> Monad c) -> Monad c
each l f k =
    Cache.each l f |> andThen k


commandWithFile :
    String
    -> List String
    -> FileOrDirectory
    -> (FileOrDirectory -> Monad a)
    -> Monad a
commandWithFile cmd args hash k =
    Cache.commandWithFile cmd args hash |> andThen k
