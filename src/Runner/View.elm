module Runner.View exposing (init)

import Command.Path exposing (Path)
import Flags exposing (Flags)
import Runner exposing (Msg, Runner, Update(..))


init : Flags -> Path -> ( Update, Cmd Msg )
init flags path =
    Runner.loadPageThen flags path (pendingPageLoad flags path)


pendingPageLoad : Flags -> Path -> Runner
pendingPageLoad flags path =
    Runner.run
        |> Runner.handlePageLoad
            (Runner.putPage flags path >> Runner.doneWith)
