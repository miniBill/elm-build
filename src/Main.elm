port module Main exposing (Flags, InnerModel, Model, Msg, Options, main)

import Ansi.Color as Ansi
import Buildfile
import ConcurrentTask exposing (ConcurrentTask, Response(..))
import ConcurrentTask.Process
import ConcurrentTask.Time
import Iso8601
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import Maybe.Extra
import Process
import Rule exposing (Path, Rule)
import Set exposing (Set)
import Task
import Time


port send : Value -> Cmd msg


port receive : (Value -> msg) -> Sub msg


port chokidar : (Value -> msg) -> Sub msg


type alias Flags =
    List String


type alias Options =
    { buildfile : String
    , watch : Bool
    }


type alias Model =
    { pool : ConcurrentTask.Pool Msg String Msg
    , options : Options
    , inner : InnerModel
    }


type InnerModel
    = Initial
    | Building
    | SettingUpChokidar { lastEvent : Time.Posix }
    | Idle { lastEvent : Time.Posix }
    | PreparingBuild


type Msg
    = WithoutTime InnerMsg
    | WithTime InnerMsg Time.Posix
    | OnProgress ( ConcurrentTask.Pool Msg String Msg, Cmd Msg )
    | OnComplete (ConcurrentTask.Response String Msg)


type InnerMsg
    = Build
    | Chokidar Value
    | SetupChokidar (Set Path)
    | GotRules (List Rule)
    | NoOp


type alias Event =
    { path : String
    , mtime : Time.Posix
    , data : EventData
    }


eventDecoder : Decoder Event
eventDecoder =
    JD.map3 Event
        (JD.field "path" JD.string)
        (JD.at [ "stats", "mtimeMs" ] (JD.map (Time.millisToPosix << round) JD.float))
        eventDataDecoder


type EventData
    = Add
    | AddDir
    | Change
    | Unlink
    | UnlinkDir


eventDataDecoder : Decoder EventData
eventDataDecoder =
    let
        inner : String -> Decoder EventData
        inner eventName =
            case eventName of
                "add" ->
                    JD.succeed Add

                "change" ->
                    JD.succeed Change

                "unlink" ->
                    JD.succeed Unlink

                "addDir" ->
                    JD.succeed AddDir

                "unlinkDir" ->
                    JD.succeed UnlinkDir

                _ ->
                    JD.fail <| "Unexpected event name: " ++ eventName
    in
    JD.field "eventName" JD.string
        |> JD.andThen inner


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        options : Options
        options =
            { buildfile = "src / Buildfile.elm"
            , watch = List.member "--watch" flags
            }
    in
    ( { pool = ConcurrentTask.pool
      , inner = Initial
      , options = options
      }
    , Task.perform (\_ -> WithoutTime Build) (Process.sleep 0)
    )


attempt : Model -> ConcurrentTask String Msg -> ( Model, Cmd Msg )
attempt model task =
    let
        ( newPool, cmd ) =
            ConcurrentTask.attempt
                { send = send
                , pool = model.pool
                , onComplete = OnComplete
                }
                task
    in
    ( { model | pool = newPool }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.inner ) of
        ( OnProgress ( newPool, cmd ), _ ) ->
            ( { model | pool = newPool }, cmd )

        ( OnComplete (UnexpectedError e), _ ) ->
            die
                { exitCode = 255
                , message = "Unexpected error: " ++ unexpectedErrorToString e
                }
                |> attempt model

        ( OnComplete (Error errorMsg), _ ) ->
            die
                { exitCode = 255
                , message = "Unexpected error: " ++ errorMsg
                }
                |> attempt model

        ( OnComplete (Success submsg), _ ) ->
            update submsg model

        ( WithoutTime inner, _ ) ->
            ( model, Task.perform (WithTime inner) Time.now )

        ( WithTime Build _, _ ) ->
            getRules model.options
                |> ConcurrentTask.andThen
                    (\rules ->
                        build rules
                            |> ConcurrentTask.map
                                (\_ ->
                                    rules
                                        |> List.foldl
                                            (\{ inputs } -> Set.union inputs)
                                            Set.empty
                                        |> SetupChokidar
                                        |> WithoutTime
                                )
                    )
                |> attempt { model | inner = Building }

        ( WithTime (SetupChokidar targets) now, Building ) ->
            if model.options.watch then
                info "Build done, setting up chokidar..."
                    |> ConcurrentTask.andThenDo (chokidarWatch <| Set.toList targets)
                    |> attempt { model | inner = SettingUpChokidar { lastEvent = now } }

            else
                info "Build done, exiting"
                    |> ConcurrentTask.andThen (\_ -> exit { exitCode = 0 })
                    |> attempt model

        ( WithTime (Chokidar data) now, SettingUpChokidar _ ) ->
            (decodeOrDie eventDecoder data <|
                \{ path } ->
                    debug ("Watching " ++ path)
                        |> ConcurrentTask.andThenDo (tickAfter 30)
            )
                |> attempt
                    { model
                        | inner =
                            SettingUpChokidar { lastEvent = now }
                    }

        ( WithTime (GotRules []) now, PreparingBuild ) ->
            info "Everything up-to-date"
                |> attempt { model | inner = Idle { lastEvent = now } }

        ( WithTime (GotRules rules) _, PreparingBuild ) ->
            info
                (String.join "\n                   " <|
                    "These rules need to be run:"
                        :: List.map viewRule rules
                )
                |> ConcurrentTask.andThenDo (ConcurrentTask.succeed (WithoutTime <| Build))
                |> attempt { model | inner = Building }

        ( WithTime (Chokidar data) now, Idle _ ) ->
            (decodeOrDie eventDecoder data <|
                \decoded ->
                    debug
                        (Ansi.fontColor Ansi.green "[Chokidar] "
                            ++ decoded.path
                            ++ " - mtime "
                            ++ Iso8601.fromTime decoded.mtime
                        )
                        |> ConcurrentTask.andThenDo (tickAfter 30)
            )
                |> attempt { model | inner = Idle { lastEvent = now } }

        ( WithTime NoOp now, SettingUpChokidar { lastEvent } ) ->
            if Time.posixToMillis now - Time.posixToMillis lastEvent >= 30 then
                info "Checking if we need to build"
                    |> prepareBuild model

            else
                ( model, Cmd.none )

        ( WithTime NoOp now, Idle { lastEvent } ) ->
            if Time.posixToMillis now - Time.posixToMillis lastEvent >= 30 then
                info "Inputs changed, checking if we need to build"
                    |> prepareBuild model

            else
                ( model, Cmd.none )

        ( WithTime NoOp _, _ ) ->
            ( model, Cmd.none )

        ( WithTime (Chokidar _) _, Building ) ->
            -- We'll check whether we need to build more after we're done building
            ( model, Cmd.none )

        ( WithTime (Chokidar _) _, PreparingBuild ) ->
            -- We'll check whether we need to build more after we're done building
            ( model, Cmd.none )

        ( WithTime _ _, _ ) ->
            die
                { message = "Unexpected (msg, model) pair " ++ Debug.toString ( msg, model.inner )
                , exitCode = 1
                }
                |> attempt model


unexpectedErrorToString : ConcurrentTask.UnexpectedError -> String
unexpectedErrorToString e =
    case e of
        ConcurrentTask.InternalError internal ->
            "Internal error: " ++ internal

        ConcurrentTask.UnhandledJsException exception ->
            "Unhandled JS exception in function " ++ exception.function ++ ": " ++ exception.message

        ConcurrentTask.ResponseDecoderFailure failure ->
            "Response decoder failure in function " ++ failure.function ++ ": " ++ JD.errorToString failure.error

        ConcurrentTask.ErrorsDecoderFailure failure ->
            "Errors decoder failure in function " ++ failure.function ++ ": " ++ JD.errorToString failure.error

        ConcurrentTask.MissingFunction function ->
            "Missing function: " ++ function


prepareBuild : Model -> ConcurrentTask String Msg -> ( Model, Cmd Msg )
prepareBuild model task =
    task
        |> ConcurrentTask.andThenDo (getRules model.options)
        |> ConcurrentTask.andThen getActiveRules
        |> ConcurrentTask.map (WithoutTime << GotRules)
        |> attempt { model | inner = PreparingBuild }


getRules : Options -> ConcurrentTask e (List Rule)
getRules options =
    Buildfile.build
        |> ConcurrentTask.map (List.map (\rule -> { rule | inputs = Set.insert options.buildfile rule.inputs }))


viewRule : Rule -> String
viewRule { inputs, outputs } =
    String.join ", " (Set.toList outputs) ++ " <-- " ++ String.join ", " (Set.toList inputs)


chokidarWatch : List Path -> ConcurrentTask e Msg
chokidarWatch paths =
    { function = "chokidarWatch"
    , expect = ConcurrentTask.expectWhatever
    , errors = ConcurrentTask.expectNoErrors
    , args = JE.list JE.string paths
    }
        |> ConcurrentTask.define
        |> ConcurrentTask.map (\_ -> WithoutTime NoOp)


build : List Rule -> ConcurrentTask String ()
build rules =
    info "Building..."
        |> ConcurrentTask.andThenDo
            (getActiveRules rules
                |> ConcurrentTask.andThen
                    (\active ->
                        active
                            |> List.map
                                (\rule ->
                                    debug ("  Running rule " ++ viewRule rule)
                                        |> ConcurrentTask.andThenDo (rule.task ())
                                )
                            |> ConcurrentTask.batch
                    )
                |> ignoreList
            )


{-| This ignores the output, but is specialized to avoid tossing away tasks.
-}
ignoreList : ConcurrentTask x (List ()) -> ConcurrentTask x ()
ignoreList =
    ConcurrentTask.map (\_ -> ())


getActiveRules : List Rule -> ConcurrentTask x (List Rule)
getActiveRules rules =
    rules
        |> ConcurrentTask.succeed
        |> thenDo (debug "Getting times")
        |> ConcurrentTask.andThen getRulesTimes
        |> thenDo (debug "Got times")
        |> ConcurrentTask.map
            (\withTime ->
                let
                    activeRules : List Rule
                    activeRules =
                        List.filterMap
                            (\( inputTimes, rule, outputTimes ) ->
                                let
                                    combine : List (Maybe Time.Posix) -> Maybe (List Int)
                                    combine times =
                                        Maybe.Extra.traverse (Maybe.map Time.posixToMillis) times
                                in
                                case ( combine inputTimes, combine outputTimes ) of
                                    ( Nothing, _ ) ->
                                        -- Input files missing: never build
                                        Nothing

                                    ( _, Nothing ) ->
                                        -- Output files missing: always build
                                        Just rule

                                    ( Just its, Just ots ) ->
                                        Maybe.andThen
                                            (\maxItime ->
                                                if List.any (\otime -> otime < maxItime) ots then
                                                    Just rule

                                                else
                                                    Nothing
                                            )
                                            (List.maximum its)
                            )
                            withTime

                    outdatedOutputs : Set Path
                    outdatedOutputs =
                        List.foldl
                            (\{ outputs } -> Set.union outputs)
                            Set.empty
                            activeRules
                in
                activeRules
                    |> List.filter
                        (\{ inputs } ->
                            inputs
                                |> Set.toList
                                |> List.all
                                    (\input ->
                                        not <|
                                            Set.member input outdatedOutputs
                                    )
                        )
            )


thenDo : ConcurrentTask e x -> ConcurrentTask e a -> ConcurrentTask e a
thenDo f xt =
    ConcurrentTask.andThen (\x -> ConcurrentTask.map (\_ -> x) f) xt


getRulesTimes : List Rule -> ConcurrentTask e (List ( List (Maybe Time.Posix), Rule, List (Maybe Time.Posix) ))
getRulesTimes rules =
    rules
        |> List.map getRuleTimes
        |> ConcurrentTask.batch


getRuleTimes : Rule -> ConcurrentTask e ( List (Maybe Time.Posix), Rule, List (Maybe Time.Posix) )
getRuleTimes rule =
    let
        getTimes : Set Path -> ConcurrentTask x (List (Maybe Time.Posix))
        getTimes set =
            set
                |> Set.toList
                |> List.map (Rule.toConcurrentTask << Rule.getMTime)
                |> ConcurrentTask.batch
    in
    ConcurrentTask.map2 (\inputTimes outputTimes -> ( inputTimes, rule, outputTimes ))
        (getTimes rule.inputs)
        (getTimes rule.outputs)


decodeOrDie : Decoder a -> Value -> (a -> ConcurrentTask e Msg) -> ConcurrentTask e Msg
decodeOrDie decoder value cont =
    case JD.decodeValue decoder value of
        Err e ->
            die
                { message = "Decoding failed: " ++ JD.errorToString e
                , exitCode = 1
                }

        Ok v ->
            cont v


die : { message : String, exitCode : Int } -> ConcurrentTask e Msg
die { message, exitCode } =
    error message
        |> ConcurrentTask.andThen (\_ -> exit { exitCode = exitCode })


exit : { exitCode : Int } -> ConcurrentTask e Msg
exit { exitCode } =
    { function = "exit"
    , expect = ConcurrentTask.expectWhatever
    , errors = ConcurrentTask.expectNoErrors
    , args = JE.object [ ( "exitCode", JE.int exitCode ) ]
    }
        |> ConcurrentTask.define
        |> ConcurrentTask.map (\_ -> WithoutTime NoOp)


info : String -> ConcurrentTask e Msg
info message =
    timedLog Ansi.cyan "info" message


debug : String -> ConcurrentTask e Msg
debug message =
    timedLog Ansi.brightBlack "debug" message


error : String -> ConcurrentTask e Msg
error message =
    timedLog Ansi.red "error" message


timedLog : Ansi.Color -> String -> String -> ConcurrentTask e Msg
timedLog color class message =
    ConcurrentTask.Time.now
        |> ConcurrentTask.andThen
            (\now ->
                ConcurrentTask.define
                    { function = "log"
                    , expect = ConcurrentTask.expectWhatever
                    , errors = ConcurrentTask.expectNoErrors
                    , args =
                        JE.string <|
                            ("["
                                ++ String.slice 11 -5 (Iso8601.fromTime now)
                                ++ "] "
                            )
                                ++ Ansi.fontColor color (String.padRight 6 ' ' class)
                                ++ message
                    }
            )
        |> ConcurrentTask.map (\_ -> WithoutTime NoOp)


tickAfter : Int -> ConcurrentTask e Msg
tickAfter delay =
    ConcurrentTask.Process.sleep delay
        |> ConcurrentTask.map (\_ -> WithoutTime NoOp)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map WithoutTime <| chokidar Chokidar
        , ConcurrentTask.onProgress
            { send = send
            , receive = receive
            , onProgress = OnProgress
            }
            model.pool
        ]
