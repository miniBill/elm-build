module PathDict exposing (PathDict, empty, foldl, fromList, get, insert, remove, size, toList, union, update)

import FastDict as Dict exposing (Dict)
import Path.Posix as Path exposing (Path)


type PathDict base kind v
    = PathDict (Dict String ( Path base kind, v ))


update : Path base kind -> (Maybe v -> Maybe v) -> PathDict base kind v -> PathDict base kind v
update key alter dict =
    case alter (get key dict) of
        Nothing ->
            remove key dict

        Just new ->
            insert key new dict


insert : Path base kind -> v -> PathDict base kind v -> PathDict base kind v
insert key v (PathDict dict) =
    PathDict (Dict.insert (Path.toString key) ( key, v ) dict)


remove : Path base kind -> PathDict base kind v -> PathDict base kind v
remove key (PathDict dict) =
    PathDict (Dict.remove (Path.toString key) dict)


get : Path base kind -> PathDict base kind v -> Maybe v
get key (PathDict dict) =
    Dict.get (Path.toString key) dict
        |> Maybe.map Tuple.second


empty : PathDict base kind v
empty =
    PathDict Dict.empty


foldl : (Path base kind -> v -> a -> a) -> a -> PathDict base kind v -> a
foldl f from (PathDict dict) =
    Dict.foldl (\_ ( k, v ) acc -> f k v acc) from dict


foldr : (Path base kind -> v -> a -> a) -> a -> PathDict base kind v -> a
foldr f from (PathDict dict) =
    Dict.foldr (\_ ( k, v ) acc -> f k v acc) from dict


fromList : List ( Path base kind, v ) -> PathDict base kind v
fromList list =
    list
        |> List.foldl
            (\(( k, _ ) as p) ->
                Dict.insert (Path.toString k) p
            )
            Dict.empty
        |> PathDict


union :
    PathDict base kind v
    -> PathDict base kind v
    -> PathDict base kind v
union (PathDict l) (PathDict r) =
    PathDict (Dict.union l r)


toList : PathDict base kind v -> List ( Path base kind, v )
toList (PathDict l) =
    Dict.values l


size : PathDict base kind v -> Int
size (PathDict d) =
    Dict.size d
