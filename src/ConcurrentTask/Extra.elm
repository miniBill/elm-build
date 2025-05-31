module ConcurrentTask.Extra exposing (Pool, attempt)

import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode


type alias Pool msg =
    ConcurrentTask.Pool msg msg msg


attempt :
    { send : Json.Decode.Value -> Cmd msg
    , onUnexpected : ConcurrentTask.UnexpectedError -> msg
    }
    -> (Result error a -> msg)
    -> ConcurrentTask error a
    -> { model | tasks : Pool msg }
    -> ( { model | tasks : Pool msg }, Cmd msg )
attempt { send, onUnexpected } toMsg task model =
    let
        mappedTask : ConcurrentTask msg msg
        mappedTask =
            task
                |> ConcurrentTask.map (\res -> toMsg (Ok res))
                |> ConcurrentTask.onError
                    (\err ->
                        toMsg (Err err)
                            |> ConcurrentTask.succeed
                    )

        onComplete : ConcurrentTask.Response msg msg -> msg
        onComplete res =
            case res of
                ConcurrentTask.Success s ->
                    s

                ConcurrentTask.Error e ->
                    e

                ConcurrentTask.UnexpectedError e ->
                    onUnexpected e

        ( tasks, newCmd ) =
            ConcurrentTask.attempt
                { pool = model.tasks
                , send = send
                , onComplete = onComplete
                }
                mappedTask
    in
    ( { model | tasks = tasks }, newCmd )
