module Runner.Move exposing (init)

import Command
import Command.Path as Path exposing (Path)
import Page exposing (Page)
import Platform.Cmd exposing (Cmd)
import Runner exposing (Msg, Runner, Update(..))
import Style
import Utilities exposing (Time)


init : Time -> Path -> Int -> Path -> ( Update, Cmd Msg )
init time sourcePath lineNumber destinationPath =
    Runner.loadPageThen time
        sourcePath
        (step1 time sourcePath lineNumber destinationPath)


step1 : Time -> Path -> Int -> Path -> Runner
step1 time sourcePath lineNumber destinationPath =
    Runner.run
        |> Runner.handlePageLoad
            (Runner.loadPageThen time destinationPath
                << step2 time sourcePath lineNumber destinationPath
            )


lineError : Path -> Page -> Int -> ( Update, Cmd Msg )
lineError path page lineNumber =
    Runner.putPage path page
        |> Runner.doneWith
        |> (String.concat
                [ "Line number "
                , Style.escape [ Style.bold ]
                , String.fromInt lineNumber
                , Style.escape [ Style.regular ]
                , " is not in the page for "
                , Style.escape [ Style.bold ]
                , Path.toTitle path
                , Style.escape [ Style.reset ]
                ]
                |> Runner.logError
           )


transfer : Time -> Path -> Int -> Path -> Page -> Page.Line -> Page -> ( Update, Cmd Msg )
transfer time sourcePath lineNumber destinationPath sourcePage sourceLine destinationPage =
    let
        modifiedDestination =
            Page.add sourceLine.bullet sourceLine.body destinationPage

        destinationLineNumber =
            List.length modifiedDestination

        destinationPathString =
            Path.toString time destinationPath

        modifiedSourceResult =
            Page.move
                destinationPathString
                destinationLineNumber
                lineNumber
                sourcePage

        mappedResult modifiedSource =
            Runner.savePageThen time destinationPath modifiedDestination <|
                step3 time sourcePath modifiedSource destinationPath modifiedDestination
    in
    modifiedSourceResult
        |> Result.map mappedResult
        |> Runner.fail (Page.lineErrorMessage <| Path.toTitle sourcePath)


step2 : Time -> Path -> Int -> Path -> Page -> Runner
step2 time sourcePath lineNumber destinationPath sourcePage =
    case Page.get lineNumber sourcePage of
        Nothing ->
            always <|
                lineError sourcePath sourcePage lineNumber

        Just sourceLine ->
            let
                doTransfer =
                    transfer
                        time
                        sourcePath
                        lineNumber
                        destinationPath
                        sourcePage
                        sourceLine
            in
            Runner.run
                |> Runner.handlePageLoad doTransfer
                |> Runner.handlePageNotFound
                    (doTransfer Page.blank
                        |> Runner.log "Creating blank page"
                        |> always
                    )


step3 : Time -> Path -> Page -> Path -> Page -> Runner
step3 time sourcePath modifiedSource destinationPath modifiedDestination =
    Runner.run
        |> Runner.handlePageSave
            (Runner.savePageThen time sourcePath modifiedSource <|
                step4 sourcePath modifiedSource destinationPath modifiedDestination
            )


step4 : Path -> Page -> Path -> Page -> Runner
step4 sourcePath modifiedSource destinationPath modifiedDestination =
    Runner.run
        |> Runner.handlePageSave
            (Runner.doneWith <|
                Cmd.batch
                    [ Runner.putPage destinationPath <| Page.clip 2 modifiedDestination
                    , Runner.putPage sourcePath <| Page.clip 1 modifiedSource
                    ]
            )
