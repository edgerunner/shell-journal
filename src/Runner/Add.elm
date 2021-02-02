module Runner.Add exposing (init)

import Bullet exposing (Bullet)
import Command.Path exposing (Path)
import Page exposing (Page)
import Runner exposing (Msg, Runner, Update)
import Utilities exposing (Time)


type alias Context =
    { time : Time
    , path : Path
    , bullet : Bullet
    , body : String
    }


init : Time -> Path -> Bullet -> String -> ( Update, Cmd Msg )
init time path bullet body =
    Runner.loadPageThen time path (step1 <| Context time path bullet body)


step1 : Context -> Runner
step1 ctx =
    let
        runWith page =
            Runner.savePageThen ctx.time ctx.path page <|
                step2 ctx page

        addTo =
            Page.add ctx.bullet ctx.body
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


step2 : Context -> Page -> Runner
step2 ctx page =
    Runner.run
        |> Runner.handlePageSave
            (Runner.doneWith <| Runner.putPage ctx.path <| Page.clip 2 page)
