module Runner.Add exposing (init)

import Bullet exposing (Bullet)
import Command.Path exposing (Path)
import Flags exposing (Flags)
import Page exposing (Page)
import Runner exposing (Msg, Runner, Update)


type alias Context =
    { flags : Flags
    , path : Path
    , bullet : Bullet
    , body : String
    }


init : Flags -> Path -> Bullet -> String -> ( Update, Cmd Msg )
init flags path bullet body =
    Runner.loadPageThen flags path (step1 <| Context flags path bullet body)


step1 : Context -> Runner
step1 ctx =
    let
        runWith page =
            Runner.savePageThen ctx.flags ctx.path page <|
                step2 ctx page

        addTo =
            Page.add ctx.bullet ctx.body
    in
    Runner.run
        |> Runner.handlePageLoadOrNew (addTo >> runWith)


step2 : Context -> Page -> Runner
step2 ctx page =
    Runner.run
        |> Runner.handlePageSave
            (Runner.doneWith <| Runner.putPage ctx.flags ctx.path <| Page.clip 2 page)
