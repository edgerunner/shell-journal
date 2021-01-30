module Runner.Move exposing (init)

import Command
import Command.Path as Path exposing (Path)
import Page exposing (Page)
import Platform.Cmd exposing (Cmd)
import Runner exposing (Msg, Runner, Update(..))
import Utilities exposing (Time)


init : Time -> Path -> Int -> Path -> ( Update, Cmd Msg )
init time sourcePath lineNumber destinationPath =
    Runner.loadPageThen time
        sourcePath
        (step1 time sourcePath lineNumber destinationPath)


step1 : Time -> Path -> Int -> Path -> Runner
step1 time sourcePath lineNumber destinationPath =
    Runner.handlePageLoad <|
        (Runner.loadPageThen time destinationPath
            << step2 time sourcePath lineNumber destinationPath
        )


step2 : Time -> Path -> Int -> Path -> Page -> Runner
step2 time sourcePath lineNumber destinationPath sourcePage =
    Runner.handlePageLoad <|
        \destinationPage ->
            case Page.get lineNumber sourcePage of
                Nothing ->
                    ( Done, Runner.put "That line doesn't exist" )

                Just sourceLine ->
                    let
                        modifiedDestination =
                            Page.add sourceLine.bullet sourceLine.body destinationPage

                        destinationLineNumber =
                            List.length modifiedDestination

                        destinationPathString =
                            Path.toString time destinationPath

                        modifiedSource =
                            Page.move
                                destinationPathString
                                destinationLineNumber
                                lineNumber
                                sourcePage
                    in
                    Runner.savePageThen time destinationPath modifiedDestination <|
                        step3 time sourcePath modifiedSource destinationPath modifiedDestination


step3 : Time -> Path -> Page -> Path -> Page -> Runner
step3 time sourcePath modifiedSource destinationPath modifiedDestination =
    Runner.handlePageSave <|
        Runner.savePageThen time sourcePath modifiedSource <|
            step4 sourcePath modifiedSource destinationPath modifiedDestination


step4 : Path -> Page -> Path -> Page -> Runner
step4 sourcePath modifiedSource destinationPath modifiedDestination =
    Runner.handlePageSave
        ( Done
        , Cmd.batch
            [ Runner.putPage destinationPath <| Page.clip 2 modifiedDestination
            , Runner.putPage sourcePath <| Page.clip 1 modifiedSource
            ]
        )
