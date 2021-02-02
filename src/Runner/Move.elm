module Runner.Move exposing (init)

import Command
import Command.Path as Path exposing (Path)
import Page exposing (Page)
import Platform.Cmd exposing (Cmd)
import Runner exposing (Msg, Runner, Update(..))
import Style
import Utilities exposing (Time)


type alias Context =
    { time : Time
    , sourcePath : Path
    , lineNumber : Int
    , destinationPath : Path
    }


init : Time -> Path -> Int -> Path -> ( Update, Cmd Msg )
init time sourcePath lineNumber destinationPath =
    if Path.toString time sourcePath == Path.toString time destinationPath then
        Runner.done
            |> Runner.logError "The origin and destination pages are the same"
            |> Runner.log
                ("The move command transfers a pending task from one page to another. "
                    ++ "Please make sure that the origin and destination pages are different."
                )

    else
        Runner.loadPageThen time
            sourcePath
            (step1 <| Context time sourcePath lineNumber destinationPath)


step1 : Context -> Runner
step1 ctx =
    Runner.run
        |> Runner.handlePageLoad
            (Runner.loadPageThen ctx.time ctx.destinationPath
                << step2 ctx
            )


lineError : Path -> Page -> Int -> ( Update, Cmd Msg )
lineError path page lineNumber =
    Runner.putPage path page
        |> Runner.doneWith
        |> (String.concat
                [ "Line number "
                , Style.escape [ Style.bold ]
                , String.fromInt lineNumber
                , Style.escape [ Style.not Style.bold ]
                , " is not in the page for "
                , Style.escape [ Style.bold ]
                , Path.toTitle path
                , Style.escape [ Style.reset ]
                ]
                |> Runner.logError
           )


transfer : Context -> Page -> Page.Line -> Page -> ( Update, Cmd Msg )
transfer ctx sourcePage sourceLine destinationPage =
    let
        modifiedDestination =
            Page.add sourceLine.bullet sourceLine.body destinationPage

        destinationLineNumber =
            List.length modifiedDestination

        destinationPathString =
            Path.toString ctx.time ctx.destinationPath

        modifiedSourceResult =
            Page.move
                destinationPathString
                destinationLineNumber
                ctx.lineNumber
                sourcePage

        mappedResult modifiedSource =
            Runner.savePageThen ctx.time ctx.destinationPath modifiedDestination <|
                step3 ctx modifiedSource modifiedDestination
    in
    modifiedSourceResult
        |> Result.map mappedResult
        |> Runner.fail (Page.lineErrorMessage <| Path.toTitle ctx.sourcePath)


step2 : Context -> Page -> Runner
step2 ctx sourcePage =
    case Page.get ctx.lineNumber sourcePage of
        Nothing ->
            always <|
                lineError ctx.sourcePath sourcePage ctx.lineNumber

        Just sourceLine ->
            let
                doTransfer =
                    transfer ctx sourcePage sourceLine
            in
            Runner.run
                |> Runner.handlePageLoad doTransfer
                |> Runner.handlePageNotFound
                    (doTransfer Page.blank
                        |> Runner.log "Creating blank page"
                        |> always
                    )


step3 : Context -> Page -> Page -> Runner
step3 ctx modifiedSource modifiedDestination =
    Runner.run
        |> Runner.handlePageSave
            (Runner.savePageThen ctx.time ctx.sourcePath modifiedSource <|
                step4 ctx modifiedSource modifiedDestination
            )


step4 : Context -> Page -> Page -> Runner
step4 ctx modifiedSource modifiedDestination =
    Runner.run
        |> Runner.handlePageSave
            (Runner.doneWith <|
                Cmd.batch
                    [ Runner.putPage ctx.destinationPath <| Page.clip 2 modifiedDestination
                    , Runner.putPage ctx.sourcePath <| Page.clip 1 modifiedSource
                    ]
            )
