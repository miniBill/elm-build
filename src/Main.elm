port module Main exposing (Command, InnerModel, Model, Msg, Options, main)

import Ansi.Color as Ansi
import Buildfile
import Cli.Option as Option exposing (Option, OptionalPositionalArgOption)
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import ConcurrentTask exposing (ConcurrentTask, Response(..))
import ConcurrentTask.Extra
import ConcurrentTask.Process
import ConcurrentTask.Time
import Internal exposing (RuleData)
import Iso8601
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import Maybe.Extra
import Process
import Rule exposing (Path, Rules)
import Set exposing (Set)
import Task
import Time
import TrackingTask


port send : Value -> Cmd msg


port receive : (Value -> msg) -> Sub msg


port chokidar : (Value -> msg) -> Sub msg


port printAndExitFailure : String -> Cmd msg


port printAndExitSuccess : String -> Cmd msg


type alias Options =
    { command : Command
    , buildFile : String
    }


type Command
    = BuildCommand
    | WatchCommand
    | DumpRulesCommand


program : Program.Config Options
program =
    Program.config
        |> Program.add
            (OptionsParser.buildSubCommand "build" (Options BuildCommand)
                |> OptionsParser.withOptionalPositionalArg buildFileOption
                |> OptionsParser.withDoc "build the project"
            )
        |> Program.add
            (OptionsParser.buildSubCommand "watch" (Options WatchCommand)
                |> OptionsParser.withOptionalPositionalArg buildFileOption
                |> OptionsParser.withDoc "build the project, then watch for changes and rebuild accordingly"
            )
        |> Program.add
            (OptionsParser.buildSubCommand "dump-rules" (Options DumpRulesCommand)
                |> OptionsParser.withOptionalPositionalArg buildFileOption
                |> OptionsParser.withDoc "write the list of rules then exit"
            )
        |> Program.add
            (OptionsParser.build (Options BuildCommand)
                |> OptionsParser.withOptionalPositionalArg buildFileOption
                |> OptionsParser.withDoc "build the project"
            )


buildFileOption : Option (Maybe String) String OptionalPositionalArgOption
buildFileOption =
    Option.optionalPositionalArg "buildfile"
        |> Option.withDefault "src/Buildfile.elm"


type alias Model =
    { tasks : ConcurrentTask.Extra.Pool Msg
    , inner : InnerModel
    }


type InnerModel
    = Initial
    | Building
    | SettingUpChokidar { lastEvent : Time.Posix }
    | Idle { lastEvent : Time.Posix }
    | PreparingBuild
    | PreparingDump


type Msg
    = WithoutTime InnerMsg
    | WithTime InnerMsg Time.Posix
    | OnProgress ( ConcurrentTask.Extra.Pool Msg, Cmd Msg )
    | OnUnexpected ConcurrentTask.UnexpectedError
    | OnComplete (Result String Msg)


type InnerMsg
    = Build
    | Chokidar Value
    | SetupChokidar (Set Path)
    | GotRules (List RuleData)
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


main : Program.StatefulProgram Model Msg Options {}
main =
    Program.stateful <|
        { printAndExitFailure = printAndExitFailure
        , printAndExitSuccess = printAndExitSuccess
        , init = init
        , config = program
        , update = update
        , subscriptions = subscriptions
        }


init : Program.FlagsIncludingArgv {} -> Options -> ( Model, Cmd Msg )
init _ _ =
    ( { tasks = ConcurrentTask.pool
      , inner = Initial
      }
    , Task.perform (\_ -> WithoutTime Build) (Process.sleep 0)
    )


attempt :
    Model
    -> ConcurrentTask String Msg
    -> ( Model, Cmd Msg )
attempt model task =
    ConcurrentTask.Extra.attempt
        { send = send
        , onUnexpected = OnUnexpected
        }
        OnComplete
        task
        model


update : Options -> Msg -> Model -> ( Model, Cmd Msg )
update options msg model =
    case ( msg, model.inner ) of
        ( OnProgress ( newPool, cmd ), _ ) ->
            ( { model | tasks = newPool }, cmd )

        ( OnUnexpected e, _ ) ->
            die ("Unexpected error: " ++ unexpectedErrorToString e)
                |> attempt model

        ( OnComplete (Err errorMsg), _ ) ->
            die ("Unexpected error: " ++ errorMsg)
                |> attempt model

        ( OnComplete (Ok submsg), _ ) ->
            update options submsg model

        ( WithoutTime inner, _ ) ->
            ( model, Task.perform (WithTime inner) Time.now )

        ( WithTime Build _, _ ) ->
            let
                (Internal.Rules inner) =
                    getRules options
            in
            case options.command of
                DumpRulesCommand ->
                    inner
                        |> ConcurrentTask.map (WithoutTime << GotRules)
                        |> attempt { model | inner = PreparingDump }

                _ ->
                    inner
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
            case options.command of
                WatchCommand ->
                    info "Build done, setting up chokidar..."
                        |> ConcurrentTask.andThenDo (chokidarWatch <| Set.toList targets)
                        |> attempt { model | inner = SettingUpChokidar { lastEvent = now } }

                BuildCommand ->
                    info "Build done, exiting"
                        |> ConcurrentTask.andThen (\_ -> exit { exitCode = 0 })
                        |> attempt model

                DumpRulesCommand ->
                    die "Unexpected state reached: SetupChokidar with a DumpRulesCommand command"
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

        ( WithTime (GotRules rules) _, PreparingDump ) ->
            info
                (String.join "\n\n" <|
                    "The rules are:"
                        :: List.map (String.join "\n" << viewRule) rules
                )
                |> ConcurrentTask.andThenDo (exit { exitCode = 0 })
                |> attempt { model | inner = PreparingDump }

        ( WithTime (GotRules []) now, PreparingBuild ) ->
            info "Everything up-to-date"
                |> attempt { model | inner = Idle { lastEvent = now } }

        ( WithTime (GotRules rules) _, PreparingBuild ) ->
            info
                ("These rules need to be run:\n"
                    ++ String.join "\n\n"
                        (List.map
                            (String.join "\n" << viewRule)
                            rules
                        )
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
                    |> prepareBuild options model

            else
                ( model, Cmd.none )

        ( WithTime NoOp now, Idle { lastEvent } ) ->
            if Time.posixToMillis now - Time.posixToMillis lastEvent >= 30 then
                info "Inputs changed, checking if we need to build"
                    |> prepareBuild options model

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

        ( WithTime inner _, _ ) ->
            die ("Unexpected (msg, model) pair " ++ innerMsgToString inner ++ " " ++ innerModelToString model.inner)
                |> attempt model


innerMsgToString : InnerMsg -> String
innerMsgToString inner =
    case inner of
        NoOp ->
            "NoOp"

        Build ->
            "Build"

        Chokidar _ ->
            "Chokidar _"

        SetupChokidar _ ->
            "SetupChokidar _"

        GotRules _ ->
            "GotRules _"


innerModelToString : InnerModel -> String
innerModelToString inner =
    case inner of
        Initial ->
            "Initial"

        Building ->
            "Building"

        SettingUpChokidar _ ->
            "SettingUpChokidar _"

        Idle _ ->
            "Idle _"

        PreparingBuild ->
            "PreparingBuild"

        PreparingDump ->
            "PreparingDump"


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


prepareBuild : Options -> Model -> ConcurrentTask String Msg -> ( Model, Cmd Msg )
prepareBuild options model task =
    task
        |> ConcurrentTask.andThenDo
            (let
                (Internal.Rules inner) =
                    getRules options
             in
             inner
            )
        |> ConcurrentTask.andThen getActiveRules
        |> ConcurrentTask.map (WithoutTime << GotRules)
        |> attempt { model | inner = PreparingBuild }


getRules : Options -> Rules
getRules options =
    Buildfile.build
        |> Rule.batch
        |> Rule.addInputs (Set.singleton options.buildFile)


viewRule : RuleData -> List String
viewRule { inputs, outputs, taskDescription } =
    [ String.join ", " (Set.toList outputs) ++ ": " ++ String.join ", " (Set.toList inputs)
    , "  " ++ String.join "\n  " taskDescription
    ]


chokidarWatch : List Path -> ConcurrentTask e Msg
chokidarWatch paths =
    { function = "chokidarWatch"
    , expect = ConcurrentTask.expectWhatever
    , errors = ConcurrentTask.expectNoErrors
    , args = JE.list JE.string paths
    }
        |> ConcurrentTask.define
        |> ConcurrentTask.map (\_ -> WithoutTime NoOp)


build : List RuleData -> ConcurrentTask String ()
build rules =
    info "Building..."
        |> ConcurrentTask.andThenDo
            (getActiveRules rules
                |> ConcurrentTask.andThen
                    (\active ->
                        active
                            |> List.map
                                (\rule ->
                                    rule.taskDescription
                                        |> List.foldl
                                            (\line ->
                                                ConcurrentTask.andThenDo (info ("Running " ++ line))
                                            )
                                            (ConcurrentTask.succeed (WithoutTime NoOp))
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


getActiveRules : List RuleData -> ConcurrentTask String (List RuleData)
getActiveRules rules =
    rules
        |> ConcurrentTask.succeed
        |> thenDo (debug "Getting times")
        |> ConcurrentTask.andThen getRulesTimes
        |> thenDo (debug "Got times")
        |> ConcurrentTask.map
            (\withTime ->
                let
                    activeRules : List RuleData
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


thenDo : ConcurrentTask String x -> ConcurrentTask String a -> ConcurrentTask String a
thenDo f xt =
    ConcurrentTask.andThen (\x -> ConcurrentTask.map (\_ -> x) f) xt


getRulesTimes : List RuleData -> ConcurrentTask String (List ( List (Maybe Time.Posix), RuleData, List (Maybe Time.Posix) ))
getRulesTimes rules =
    rules
        |> List.map getRuleTimes
        |> ConcurrentTask.batch


getRuleTimes : RuleData -> ConcurrentTask String ( List (Maybe Time.Posix), RuleData, List (Maybe Time.Posix) )
getRuleTimes rule =
    let
        getTimes : Set Path -> ConcurrentTask String (List (Maybe Time.Posix))
        getTimes set =
            set
                |> Set.toList
                |> List.map (TrackingTask.toConcurrentTask << Rule.getMTime)
                |> ConcurrentTask.batch
    in
    ConcurrentTask.map2 (\inputTimes outputTimes -> ( inputTimes, rule, outputTimes ))
        (getTimes rule.inputs)
        (getTimes rule.outputs)


decodeOrDie : Decoder a -> Value -> (a -> ConcurrentTask e Msg) -> ConcurrentTask e Msg
decodeOrDie decoder value cont =
    case JD.decodeValue decoder value of
        Err e ->
            die <| "Decoding failed: " ++ JD.errorToString e

        Ok v ->
            cont v


die : String -> ConcurrentTask e Msg
die message =
    error message
        |> ConcurrentTask.andThenDo (exit { exitCode = 1 })


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
            model.tasks
        ]
