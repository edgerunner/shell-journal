module Runner.View exposing (init)

import Command.Path as Path exposing (Path)
import Page
import Runner exposing (Msg, Update(..))
import Utilities exposing (Time)


init : Time -> Path -> ( Update, Cmd Msg )
init time path =
    Runner.loadPageThen time path (pendingPageLoad path)


pendingPageLoad : Path -> Msg -> ( Update, Cmd Msg )
pendingPageLoad path =
    Runner.handlePageLoad
        (\page ->
            ( Done
            , Cmd.batch
                [ Runner.put <| Path.toTitle path
                , Runner.put <| Page.terminalOutput page
                ]
            )
        )
