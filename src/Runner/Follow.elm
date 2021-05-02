module Runner.Follow exposing (init)

import Bullet
import Command.Path exposing (Path)
import Flags exposing (Flags)
import Page
import Runner exposing (Msg, Runner, Update)


init : Flags -> Path -> Int -> ( Update, Cmd Msg )
init flags path lineNumber =
    Runner.loadPageThen flags path (step1 flags lineNumber)


step1 : Flags -> Int -> Runner
step1 flags lineNumber =
    let
        withTarget target =
            Runner.loadPageThen flags
                target
                (step2 flags target)

        withPage page =
            Page.get lineNumber page
                |> Maybe.map .bullet
                |> Maybe.andThen Bullet.target
                |> Maybe.map withTarget
                |> Maybe.withDefault notFound

        notFound =
            Runner.logError "Can't follow that. There's no link there" (Runner.done 1)
    in
    Runner.run
        |> Runner.handlePageLoad withPage


step2 : Flags -> Path -> Runner
step2 flags path =
    Runner.run
        |> Runner.handlePageLoad
            (Runner.putPage flags path >> Runner.doneWith)
