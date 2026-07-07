module CommandOptions exposing (CommandOptions, allowNon0Status, default, toStreamCommandOptions, toStringList, withOutput)

import BackendTask.Stream as Stream
import Maybe.Extra


default : CommandOptions
default =
    { allowNon0Status = False
    , output = Nothing
    }


type alias CommandOptions =
    { allowNon0Status : Bool, output : Maybe Stream.StderrOutput }


toStreamCommandOptions : CommandOptions -> Stream.CommandOptions
toStreamCommandOptions options =
    Stream.defaultCommandOptions
        |> (if options.allowNon0Status then
                Stream.allowNon0Status

            else
                identity
           )
        |> (case options.output of
                Just output ->
                    Stream.withOutput output

                Nothing ->
                    identity
           )


{-| Convert the `CommandOptions` into a list of string for use in hashing tasks.
-}
toStringList : CommandOptions -> List String
toStringList options =
    [ if options.allowNon0Status then
        Just "allowNon0Status"

      else
        Nothing
    , Maybe.map stderrOutputToString options.output
    ]
        |> Maybe.Extra.values


stderrOutputToString : Stream.StderrOutput -> String
stderrOutputToString output =
    case output of
        Stream.MergeStderrAndStdout ->
            "MergeStderrAndStdout"

        Stream.PrintStderr ->
            "PrintStderr"

        Stream.IgnoreStderr ->
            "IgnoreStderr"

        Stream.StderrInsteadOfStdout ->
            "StderrInsteadOfStdout"


allowNon0Status : CommandOptions -> CommandOptions
allowNon0Status options =
    { options | allowNon0Status = True }


withOutput : Stream.StderrOutput -> CommandOptions -> CommandOptions
withOutput output options =
    { options | output = Just output }
