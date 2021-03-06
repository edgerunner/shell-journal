module Runner.LoadModifySavePage exposing (init, step1, step2)

import Command.Path as Path exposing (Path)
import Flags exposing (Flags)
import Page exposing (LineError, Page)
import Runner exposing (Msg, Runner, Update(..), handlePageLoad, handlePageSave, loadPageThen, savePageThen)


init : Flags -> Path -> (Page -> Result LineError Page) -> ( Update, Cmd Msg )
init flags path modify =
    loadPageThen flags path (step1 flags path modify)


step1 : Flags -> Path -> (Page -> Result LineError Page) -> Runner
step1 flags path modify =
    Runner.run
        |> handlePageLoad
            (modify
                >> Result.map
                    (\modifiedPage ->
                        savePageThen flags path modifiedPage (step2 flags path modifiedPage)
                    )
                >> Runner.fail (Page.lineErrorMessage <| Path.toTitle flags path)
            )


step2 : Flags -> Path -> Page -> Runner
step2 flags path page =
    Runner.run
        |> handlePageSave
            (Runner.doneWith <| Runner.putPage flags path (Page.clip 2 page))
