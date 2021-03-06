module Runner.Move exposing (init)

import Command.Path as Path exposing (Path)
import Flags exposing (Flags)
import Page exposing (Page)
import Platform.Cmd exposing (Cmd)
import Runner exposing (Msg, Runner, Update(..))
import Style


type alias Context =
    { flags : Flags
    , sourcePath : Path
    , lineNumber : Int
    , destinationPath : Path
    }


init : Flags -> Path -> Int -> Path -> ( Update, Cmd Msg )
init flags sourcePath lineNumber destinationPath =
    if Path.toString flags sourcePath == Path.toString flags destinationPath then
        Runner.done 1
            |> Runner.logError "The origin and destination pages are the same"
            |> Runner.log
                ("The move command transfers a pending task from one page to another. "
                    ++ "Please make sure that the origin and destination pages are different."
                )

    else
        Runner.loadPageThen flags
            sourcePath
            (step1 <| Context flags sourcePath lineNumber destinationPath)


step1 : Context -> Runner
step1 ctx =
    Runner.run
        |> Runner.handlePageLoad
            (Runner.loadPageThen ctx.flags ctx.destinationPath
                << step2 ctx
            )


lineError : Flags -> Path -> Page -> Int -> ( Update, Cmd Msg )
lineError flags path page lineNumber =
    Runner.putPage flags path page
        |> Runner.doneWith
        |> (String.concat
                [ "Line number "
                , Style.escape [ Style.bold ]
                , String.fromInt lineNumber
                , Style.escape [ Style.not Style.bold ]
                , " is not in the page for "
                , Style.escape [ Style.bold ]
                , Path.toTitle flags path
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
            Path.toString ctx.flags ctx.destinationPath

        modifiedSourceResult =
            Page.move
                destinationPathString
                destinationLineNumber
                ctx.lineNumber
                sourcePage

        mappedResult modifiedSource =
            Runner.savePageThen ctx.flags ctx.destinationPath modifiedDestination <|
                step3 ctx modifiedSource modifiedDestination
    in
    modifiedSourceResult
        |> Result.map mappedResult
        |> Runner.fail (Page.lineErrorMessage <| Path.toTitle ctx.flags ctx.sourcePath)


step2 : Context -> Page -> Runner
step2 ctx sourcePage =
    case Page.get ctx.lineNumber sourcePage of
        Nothing ->
            always <|
                lineError ctx.flags ctx.sourcePath sourcePage ctx.lineNumber

        Just sourceLine ->
            Runner.run
                |> Runner.handlePageLoad
                    (transfer ctx sourcePage sourceLine)
                |> Runner.handlePageNotFound
                    (always <| transfer ctx sourcePage sourceLine Page.blank)


step3 : Context -> Page -> Page -> Runner
step3 ctx modifiedSource modifiedDestination =
    Runner.run
        |> Runner.handlePageSave
            (Runner.savePageThen ctx.flags ctx.sourcePath modifiedSource <|
                step4 ctx modifiedSource modifiedDestination
            )


step4 : Context -> Page -> Page -> Runner
step4 ctx modifiedSource modifiedDestination =
    Runner.run
        |> Runner.handlePageSave
            (Runner.doneWith <|
                Cmd.batch
                    [ Runner.putPage ctx.flags ctx.destinationPath <| Page.clip 2 modifiedDestination
                    , Runner.putPage ctx.flags ctx.sourcePath <| Page.clip 1 modifiedSource
                    ]
            )
