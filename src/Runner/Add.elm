module Runner.Add exposing (init)

import Bullet exposing (Bullet)
import Command.Path exposing (Path)
import Page exposing (Page)
import Runner exposing (Msg, Runner, Update)
import Utilities exposing (Time)


init : Time -> Path -> Bullet -> String -> ( Update, Cmd Msg )
init time path bullet content =
    Runner.loadPageThen time path (step1 time path bullet content)


step1 : Time -> Path -> Bullet -> String -> Runner
step1 time path bullet body =
    let
        runWith page =
            Runner.savePageThen time path page <|
                step3 path page

        addTo =
            Page.add bullet body
    in
    Runner.run
        |> Runner.handlePageLoad (addTo >> runWith)
        |> Runner.handlePageNotFound
            (always
                (Page.blank
                    |> addTo
                    |> runWith
                    |> Runner.log "Opening a new page"
                )
            )


step3 : Path -> Page -> Runner
step3 path page =
    Runner.run
        |> Runner.handlePageSave
            (Runner.doneWith <| Runner.putPage path <| Page.clip 2 page)
