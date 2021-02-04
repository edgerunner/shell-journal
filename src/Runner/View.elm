module Runner.View exposing (init)

import Command.Path exposing (Path)
import Flags exposing (Flags)
import Runner exposing (Msg, Runner, Update(..))


init : Flags -> Path -> ( Update, Cmd Msg )
init time path =
    Runner.loadPageThen time path (pendingPageLoad path)


pendingPageLoad : Path -> Runner
pendingPageLoad path =
    Runner.run
        |> Runner.handlePageLoad
            (Runner.putPage path >> Runner.doneWith)
