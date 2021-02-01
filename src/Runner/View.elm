module Runner.View exposing (init)

import Command.Path exposing (Path)
import Runner exposing (Msg, Runner, Update(..))
import Utilities exposing (Time)


init : Time -> Path -> ( Update, Cmd Msg )
init time path =
    Runner.loadPageThen time path (pendingPageLoad path)


pendingPageLoad : Path -> Runner
pendingPageLoad path =
    Runner.run
        |> Runner.handlePageLoad
            (Runner.putPage path >> Runner.doneWith)
