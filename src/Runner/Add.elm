module Runner.Add exposing (init)

import Bullet exposing (Bullet)
import Command.Path exposing (Path)
import Page
import Runner exposing (Msg, Update)
import Runner.LoadModifySavePage as LMSP
import Utilities exposing (Time)


init : Time -> Path -> Bullet -> String -> ( Update, Cmd Msg )
init time path bullet content =
    LMSP.init time path (Page.add bullet content)
