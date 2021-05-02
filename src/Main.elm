module Main exposing (main)

import Command exposing (Command(..))
import Command.Path exposing (Path(..))
import Flags exposing (Flags)
import Json.Decode exposing (Value)
import Runner exposing (Msg, Update)
import Runner.Add
import Runner.Check
import Runner.Follow
import Runner.List
import Runner.Move
import Runner.Star
import Runner.Strike
import Runner.View


main : Program Value Update Msg
main =
    Platform.worker
        { init = init
        , update = Runner.update
        , subscriptions = Runner.subscriptions
        }


init : Value -> ( Update, Cmd Msg )
init rawFlags =
    Flags.decode Command.parse rawFlags
        |> Result.map selectCommand
        |> Runner.fail (always "Sorry, that command did not make sense")


selectCommand : ( Command, Flags ) -> ( Update, Cmd Msg )
selectCommand ( command, flags ) =
    case command of
        View path ->
            Runner.View.init flags path

        Add path bullet body ->
            Runner.Add.init flags path bullet body

        Check path lineNumber ->
            Runner.Check.init flags path lineNumber

        Star path lineNumber ->
            Runner.Star.init flags path lineNumber

        Strike path lineNumber ->
            Runner.Strike.init flags path lineNumber

        Move sourcePath lineNumber destinationPath ->
            Runner.Move.init flags sourcePath lineNumber destinationPath

        List _ ->
            Runner.List.init flags

        Follow path lineNumber ->
            Runner.Follow.init flags path lineNumber
