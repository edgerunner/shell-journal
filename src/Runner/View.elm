module Runner.View exposing (init)

import Command.Path exposing (Path)
import Runner exposing (Msg, Update(..))
import Utilities exposing (Time)


init : Time -> Path -> ( Update, Cmd Msg )
init time path =
    Runner.loadPageThen time path (pendingPageLoad path)


pendingPageLoad : Path -> Msg -> ( Update, Cmd Msg )
pendingPageLoad path =
    Runner.handlePageLoad
        (Runner.putPage path >> Tuple.pair Done)