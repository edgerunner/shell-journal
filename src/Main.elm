module Main exposing (main)

import Command exposing (Command(..))
import Command.Path exposing (Path(..))
import Flags exposing (Flags)
import Runner exposing (Msg, Update)
import Runner.Add
import Runner.Check
import Runner.Move
import Runner.Star
import Runner.View
import Utilities exposing (Time)


main : Program Flags Update Msg
main =
    Platform.worker
        { init = init
        , update = Runner.update
        , subscriptions = Runner.subscriptions
        }


init : Flags -> ( Update, Cmd Msg )
init flags =
    Flags.decode flags
        |> Result.map selectCommand
        |> Runner.fail (always "Sorry, that command did not make sense")


selectCommand : ( Time, Command ) -> ( Update, Cmd Msg )
selectCommand ( time, command ) =
    case command of
        View path ->
            Runner.View.init time path

        Add path bullet body ->
            Runner.Add.init time path bullet body

        Check path lineNumber ->
            Runner.Check.init time path lineNumber

        Star path lineNumber ->
            Runner.Star.init time path lineNumber

        Move sourcePath lineNumber destinationPath ->
            Runner.Move.init time sourcePath lineNumber destinationPath
