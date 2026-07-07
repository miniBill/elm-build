module CommandOptions exposing (CommandOptions, allowNon0Status, default, toStreamCommandOptions, toStringList)

import BackendTask.Stream as Stream
import Maybe.Extra


default : CommandOptions
default =
    { allowNon0Status = False }


type alias CommandOptions =
    { allowNon0Status : Bool }


toStreamCommandOptions : CommandOptions -> Stream.CommandOptions
toStreamCommandOptions options =
    Stream.defaultCommandOptions
        |> (if options.allowNon0Status then
                Stream.allowNon0Status

            else
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
    ]
        |> Maybe.Extra.values


allowNon0Status : CommandOptions -> CommandOptions
allowNon0Status options =
    { options | allowNon0Status = True }
