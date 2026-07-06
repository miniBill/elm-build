module BuildTask.Do exposing (all, allowFatal, andThen, combine, each, jobs, map, map2, map3, map4, map5, withFile, writeFile)

import BackendTask.File as File
import BuildTask exposing (BuildTask, FileOrDirectory)
import FatalError exposing (FatalError)
import Pages.Script as Script


andThen : (b -> BuildTask e a) -> BuildTask e b -> BuildTask e a
andThen =
    BuildTask.andThen


combine : List (BuildTask e a) -> (List a -> BuildTask e b) -> BuildTask e b
combine inputs k =
    BuildTask.combine inputs |> andThen k


all : (a -> BuildTask e b) -> List a -> (List b -> BuildTask e c) -> BuildTask e c
all f inputs k =
    BuildTask.combine (List.map f inputs) |> andThen k


map :
    (a -> i)
    -> BuildTask e a
    -> (i -> BuildTask e r)
    -> BuildTask e r
map f a k =
    BuildTask.map f a |> andThen k


map2 :
    (a -> b -> i)
    -> BuildTask e a
    -> BuildTask e b
    -> (i -> BuildTask e r)
    -> BuildTask e r
map2 f a b k =
    BuildTask.map2 f a b |> andThen k


map3 :
    (a -> b -> c -> i)
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
    -> (i -> BuildTask e r)
    -> BuildTask e r
map3 f a b c k =
    BuildTask.map3 f a b c |> andThen k


map4 :
    (a -> b -> c -> d -> i)
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
    -> BuildTask e d
    -> (i -> BuildTask e r)
    -> BuildTask e r
map4 f a b c d k =
    BuildTask.map4 f a b c d |> andThen k


map5 :
    (a -> b -> c -> d -> f -> i)
    -> BuildTask e a
    -> BuildTask e b
    -> BuildTask e c
    -> BuildTask e d
    -> BuildTask e f
    -> (i -> BuildTask e r)
    -> BuildTask e r
map5 f a b c d e k =
    BuildTask.map5 f a b c d e |> andThen k


writeFile :
    String
    -> (FileOrDirectory -> BuildTask { fatal : FatalError, recoverable : Script.Error } a)
    -> BuildTask { fatal : FatalError, recoverable : Script.Error } a
writeFile content k =
    BuildTask.writeFile content |> andThen k


jobs : (Int -> BuildTask e a) -> BuildTask e a
jobs k =
    BuildTask.jobs |> andThen k


withFile :
    FileOrDirectory
    -> (String -> BuildTask { fatal : FatalError, recoverable : File.FileReadError decoderError } b)
    -> (b -> BuildTask { fatal : FatalError, recoverable : File.FileReadError decoderError } a)
    -> BuildTask { fatal : FatalError, recoverable : File.FileReadError decoderError } a
withFile hash f k =
    BuildTask.withFile hash f |> andThen k


each : List a -> (a -> BuildTask e b) -> (List b -> BuildTask e c) -> BuildTask e c
each l f k =
    BuildTask.each l f |> andThen k


allowFatal : BuildTask { e | fatal : FatalError } b -> (b -> BuildTask FatalError a) -> BuildTask FatalError a
allowFatal t k =
    BuildTask.allowFatal t |> andThen k
