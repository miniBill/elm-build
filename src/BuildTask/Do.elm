module BuildTask.Do exposing (all, allowFatal, andThen, combine, each, jobs, map, map2, map3, map4, map5, withFile, writeFile)

import BackendTask.File as File
import BuildTask exposing (BuildTask, FileOrDirectory)
import FatalError exposing (FatalError)
import Pages.Script as Script


andThen : (b -> BuildTask tools e a) -> BuildTask tools e b -> BuildTask tools e a
andThen =
    BuildTask.andThen


combine : List (BuildTask tools e a) -> (List a -> BuildTask tools e b) -> BuildTask tools e b
combine inputs k =
    BuildTask.combine inputs |> andThen k


all : (a -> BuildTask tools e b) -> List a -> (List b -> BuildTask tools e c) -> BuildTask tools e c
all f inputs k =
    BuildTask.combine (List.map f inputs) |> andThen k


map :
    (a -> i)
    -> BuildTask tools e a
    -> (i -> BuildTask tools e r)
    -> BuildTask tools e r
map f a k =
    BuildTask.map f a |> andThen k


map2 :
    (a -> b -> i)
    -> BuildTask tools e a
    -> BuildTask tools e b
    -> (i -> BuildTask tools e r)
    -> BuildTask tools e r
map2 f a b k =
    BuildTask.map2 f a b |> andThen k


map3 :
    (a -> b -> c -> i)
    -> BuildTask tools e a
    -> BuildTask tools e b
    -> BuildTask tools e c
    -> (i -> BuildTask tools e r)
    -> BuildTask tools e r
map3 f a b c k =
    BuildTask.map3 f a b c |> andThen k


map4 :
    (a -> b -> c -> d -> i)
    -> BuildTask tools e a
    -> BuildTask tools e b
    -> BuildTask tools e c
    -> BuildTask tools e d
    -> (i -> BuildTask tools e r)
    -> BuildTask tools e r
map4 f a b c d k =
    BuildTask.map4 f a b c d |> andThen k


map5 :
    (a -> b -> c -> d -> f -> i)
    -> BuildTask tools e a
    -> BuildTask tools e b
    -> BuildTask tools e c
    -> BuildTask tools e d
    -> BuildTask tools e f
    -> (i -> BuildTask tools e r)
    -> BuildTask tools e r
map5 f a b c d e k =
    BuildTask.map5 f a b c d e |> andThen k


writeFile :
    String
    -> (FileOrDirectory -> BuildTask tools { fatal : FatalError, recoverable : Script.Error } a)
    -> BuildTask tools { fatal : FatalError, recoverable : Script.Error } a
writeFile content k =
    BuildTask.writeFile content |> andThen k


jobs : (Int -> BuildTask tools e a) -> BuildTask tools e a
jobs k =
    BuildTask.jobs |> andThen k


withFile :
    FileOrDirectory
    -> (String -> BuildTask tools { fatal : FatalError, recoverable : File.FileReadError decoderError } b)
    -> (b -> BuildTask tools { fatal : FatalError, recoverable : File.FileReadError decoderError } a)
    -> BuildTask tools { fatal : FatalError, recoverable : File.FileReadError decoderError } a
withFile hash f k =
    BuildTask.withFile hash f |> andThen k


each : List a -> (a -> BuildTask tools e b) -> (List b -> BuildTask tools e c) -> BuildTask tools e c
each l f k =
    BuildTask.each l f |> andThen k


allowFatal : BuildTask tools { e | fatal : FatalError } b -> (b -> BuildTask tools FatalError a) -> BuildTask tools FatalError a
allowFatal t k =
    BuildTask.allowFatal t |> andThen k
