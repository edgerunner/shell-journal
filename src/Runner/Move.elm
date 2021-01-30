module Runner.Move exposing (init)

import Bullet exposing (Bullet)
import Command
import Command.Path as Path exposing (Path)
import Page
import Runner exposing (Msg, Update(..))
import Runner.LoadModifySavePage as LMSP
import Utilities exposing (Time)


init : Time -> Path -> Int -> Path -> ( Update, Cmd Msg )
init time sourcePath lineNumber destinationPath =
    Runner.loadPageThen time
        sourcePath
        (step1 sourcePath time lineNumber destinationPath)


step1 : Path -> Time -> Int -> Path -> Msg -> ( Update, Cmd Msg )
step1 sourcePath time lineNumber destinationPath =
    Runner.handlePageLoad
        (\page ->
            let
                maybeLine =
                    Page.get lineNumber page

                updatedPage =
                    Page.move (Path.toString time destinationPath) lineNumber page
            in
            case maybeLine of
                Just line ->
                    Runner.savePageThen time sourcePath updatedPage (step2 time destinationPath line.bullet line.body)

                Nothing ->
                    ( Done, Runner.put "That line number does not exist in the source page" )
        )


step2 : Time -> Path -> Bullet -> String -> Msg -> ( Update, Cmd Msg )
step2 time destinationPath bullet body =
    Runner.handlePageSave <|
        LMSP.init time destinationPath (Page.add bullet body)
