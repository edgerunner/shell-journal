module Runner.Star exposing (init)

import Command.Path exposing (Path)
import Page
import Runner exposing (Msg, Update)
import Runner.LoadModifySavePage as LMSP
import Utilities exposing (Time)


init : Time -> Path -> Int -> ( Update, Cmd Msg )
init time path lineNumber =
    LMSP.init time path (Page.star lineNumber)
