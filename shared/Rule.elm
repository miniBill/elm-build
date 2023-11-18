module Rule exposing (Path, Rule, TrackingTask, andThen, batch, combineMap, command, getMTime, getSize, listFiles, map, map2, toConcurrentTask, writeCodegenFile, writeFile)

import ConcurrentTask exposing (ConcurrentTask)
import Elm
import Json.Decode as JD
import Json.Encode as JE
import Set exposing (Set)
import Time


type alias Path =
    String


type alias Rule =
    { inputs : Set Path
    , outputs : Set Path
    , task : () -> ConcurrentTask String ()
    , taskDescription : String
    }


do :
    TrackingTask a
    -> List Path
    -> (a -> String)
    -> (a -> ConcurrentTask String ())
    -> ConcurrentTask e Rule
do (TrackingTask task) outputs desc f =
    ConcurrentTask.map
        (\( inputs, a ) ->
            { inputs = Set.fromList inputs
            , outputs = Set.fromList outputs
            , task = \_ -> f a
            , taskDescription = desc a
            }
        )
        (ConcurrentTask.mapError never task)


listFiles : Path -> TrackingTask (List Path)
listFiles path =
    { function = "listFiles"
    , expect = ConcurrentTask.expectJson (JD.list JD.string)
    , errors = ConcurrentTask.expectNoErrors
    , args = JE.string path
    }
        |> ConcurrentTask.define
        |> ConcurrentTask.map (\v -> ( [ path ], v ))
        |> TrackingTask


{-| Runs a command. `additionalInputs` should be list of files that the command will read. You don't need to specify files that were already read to create the rule, but it's fine to do so.

You should wrap this in a function that automatically keeps track of inputs and outputs.

-}
command :
    TrackingTask a
    ->
        { outputs : List Path
        , additionalInputs : List Path
        , toCommand : a -> String
        }
    -> ConcurrentTask e Rule
command (TrackingTask task) config =
    do
        (task
            |> ConcurrentTask.map
                (\( inputs, value ) ->
                    ( config.additionalInputs ++ inputs, value )
                )
            |> TrackingTask
        )
        config.outputs
        config.toCommand
        (\a ->
            let
                commandString : String
                commandString =
                    config.toCommand a
            in
            { args = JE.string commandString
            , errors = ConcurrentTask.expectNoErrors
            , expect = ConcurrentTask.expectJson JD.int
            , function = "command"
            }
                |> ConcurrentTask.define
                |> ConcurrentTask.andThen
                    (\exitCode ->
                        if exitCode == 0 then
                            ConcurrentTask.succeed ()

                        else
                            ConcurrentTask.fail
                                ("Command "
                                    ++ JE.encode 0 (JE.string commandString)
                                    ++ " failed with exit code "
                                    ++ String.fromInt exitCode
                                )
                    )
        )


writeFile :
    Path
    -> TrackingTask a
    -> (a -> String)
    -> ConcurrentTask e Rule
writeFile path task content =
    do task
        [ path ]
        (\_ -> "writing file " ++ path)
        (\a ->
            { function = "writeFile"
            , expect = ConcurrentTask.expectWhatever
            , errors = ConcurrentTask.expectNoErrors
            , args =
                JE.object
                    [ ( "path", JE.string path )
                    , ( "content", JE.string (content a) )
                    ]
            }
                |> ConcurrentTask.define
        )


writeCodegenFile :
    TrackingTask a
    -> List String
    -> (a -> List Elm.Declaration)
    -> ConcurrentTask e Rule
writeCodegenFile task moduleName declarations =
    let
        targetName : String
        targetName =
            String.join "/" ("generated" :: moduleName) ++ ".elm"
    in
    writeFile targetName
        task
        (\a -> (Elm.file moduleName (declarations a)).contents)
        |> ConcurrentTask.map (\rule -> { rule | taskDescription = "elm-codegen => " ++ targetName })


getMTime : Path -> TrackingTask (Maybe Time.Posix)
getMTime path =
    stat path |> map (Maybe.map <| \{ mtime } -> mtime)


getSize : Path -> TrackingTask (Maybe Int)
getSize path =
    stat path |> map (Maybe.map <| \{ size } -> size)


type alias Stat =
    { size : Int
    , mtime : Time.Posix
    }


stat : String -> TrackingTask (Maybe Stat)
stat path =
    TrackingTask
        ({ function = "stat"
         , expect = ConcurrentTask.expectJson (JD.nullable statDecoder)
         , errors = ConcurrentTask.expectNoErrors
         , args = JE.string path
         }
            |> ConcurrentTask.define
            |> ConcurrentTask.map (\res -> ( [ path ], res ))
        )


statDecoder : JD.Decoder Stat
statDecoder =
    JD.map2 Stat
        (JD.field "size" JD.int)
        (JD.map (Time.millisToPosix << round) <| JD.field "mtimeMs" JD.float)



-- TrackingTask --


type TrackingTask a
    = TrackingTask (ConcurrentTask Never ( List Path, a ))


succeed : a -> TrackingTask a
succeed value =
    TrackingTask (ConcurrentTask.succeed ( [], value ))


map : (a -> b) -> TrackingTask a -> TrackingTask b
map f (TrackingTask task) =
    TrackingTask (ConcurrentTask.map (\( inputs, x ) -> ( inputs, f x )) task)


map2 : (a -> b -> c) -> TrackingTask a -> TrackingTask b -> TrackingTask c
map2 f (TrackingTask ltask) (TrackingTask rtask) =
    TrackingTask
        (ConcurrentTask.map2
            (\( linputs, lx ) ( rinputs, rx ) ->
                ( linputs ++ rinputs, f lx rx )
            )
            ltask
            rtask
        )


andThen : (a -> TrackingTask b) -> TrackingTask a -> TrackingTask b
andThen f (TrackingTask task) =
    task
        |> ConcurrentTask.andThen
            (\( xi, x ) ->
                let
                    (TrackingTask fx) =
                        f x
                in
                ConcurrentTask.map
                    (\( fxi, v ) ->
                        ( fxi ++ xi
                        , v
                        )
                    )
                    fx
            )
        |> TrackingTask


batch : List (TrackingTask a) -> TrackingTask (List a)
batch list =
    List.foldl
        (\item acc -> map2 (::) item acc)
        (succeed [])
        list


combineMap : (a -> TrackingTask b) -> List a -> TrackingTask (List b)
combineMap f list =
    List.foldl
        (\item acc -> map2 (::) (f item) acc)
        (succeed [])
        list


toConcurrentTask : TrackingTask a -> ConcurrentTask e a
toConcurrentTask (TrackingTask task) =
    task
        |> ConcurrentTask.mapError never
        |> ConcurrentTask.map Tuple.second
