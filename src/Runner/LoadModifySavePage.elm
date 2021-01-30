module Runner.LoadModifySavePage exposing (init, step1, step2)

import Command.Path exposing (Path)
import Page exposing (Page)
import Runner exposing (Msg, Update(..), handlePageLoad, handlePageSave, loadPageThen, savePageThen)
import Utilities exposing (Time)


init : Time -> Path -> (Page -> Page) -> ( Update, Cmd Msg )
init time path modify =
    loadPageThen time path (step1 time path modify)


step1 : Time -> Path -> (Page -> Page) -> Msg -> ( Update, Cmd Msg )
step1 time path modify =
    handlePageLoad
        (\page ->
            let
                modifiedPage =
                    modify page
            in
            savePageThen time path modifiedPage (step2 path modifiedPage)
        )


step2 : Path -> Page -> Msg -> ( Update, Cmd Msg )
step2 path page =
    handlePageSave
        ( Done
        , Runner.putPage path (Page.clip 2 page)
        )
