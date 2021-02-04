module Runner.Check exposing (init)

import Command.Path exposing (Path)
import Flags exposing (Flags)
import Page
import Runner exposing (Msg, Update)
import Runner.LoadModifySavePage as LMSP


init : Flags -> Path -> Int -> ( Update, Cmd Msg )
init flags path lineNumber =
    LMSP.init flags path (Page.check lineNumber)
