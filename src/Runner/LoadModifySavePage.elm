module Runner.LoadModifySavePage exposing (init, step1, step2)

import Command.Path as Path exposing (Path)
import Page exposing (LineError, Page)
import Runner exposing (Msg, Runner, Update(..), handlePageLoad, handlePageSave, loadPageThen, savePageThen)
import Utilities exposing (Time)


init : Time -> Path -> (Page -> Result LineError Page) -> ( Update, Cmd Msg )
init time path modify =
    loadPageThen time path (step1 time path modify)


step1 : Time -> Path -> (Page -> Result LineError Page) -> Runner
step1 time path modify =
    Runner.run
        |> handlePageLoad
            (modify
                >> Result.map
                    (\modifiedPage ->
                        savePageThen time path modifiedPage (step2 path modifiedPage)
                    )
                >> Runner.fail (Page.lineErrorMessage <| Path.toTitle path)
            )


step2 : Path -> Page -> Runner
step2 path page =
    Runner.run
        |> handlePageSave
            (Runner.doneWith <| Runner.putPage path (Page.clip 2 page))
