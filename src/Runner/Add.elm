module Runner.Add exposing (init)

import Bullet exposing (Bullet)
import Command.Path exposing (Path)
import FS
import Page exposing (Page)
import Runner exposing (Error(..), Msg, Runner, Success(..), Update(..))
import Utilities exposing (Time)


init : Time -> Path -> Bullet -> String -> ( Update, Cmd Msg )
init time path bullet content =
    Runner.loadPageThen time path (step1 time path bullet content)


step1 : Time -> Path -> Bullet -> String -> Runner
step1 time path bullet body msg =
    let
        runWith page =
            Runner.savePageThen time path page <|
                step3 path page

        addTo =
            Page.add bullet body
    in
    case msg of
        Ok (LoadedPage page) ->
            runWith <| addTo page

        Err (FilesystemError (FS.NotFound _)) ->
            Page.blank
                |> addTo
                |> runWith
                |> Runner.log "Opening a new page"

        _ ->
            ( Done, Runner.put "Invalid transition. This is a bug" )


step3 : Path -> Page -> Runner
step3 path page =
    Runner.handlePageSave
        ( Done
        , Runner.putPage path <| Page.clip 2 page
        )
