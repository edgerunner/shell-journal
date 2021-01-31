module Runner.LoadModifySavePage exposing (init, step1, step2)

import Command.Path as Path exposing (Path)
import Page exposing (LineError, Page)
import Runner exposing (Msg, Runner, Update(..), handlePageLoad, handlePageSave, loadPageThen, savePageThen)
import Utilities exposing (Time)


init : Time -> Path -> (Page -> Result LineError Page) -> ( Update, Cmd Msg )
init time path modify =
    loadPageThen time path (step1 time path modify)


step1 : Time -> Path -> (Page -> Result LineError Page) -> Runner
step1 time path modify =
    Runner.run
        |> handlePageLoad
            (modify
                >> Result.map
                    (\modifiedPage ->
                        savePageThen time path modifiedPage (step2 path modifiedPage)
                    )
                >> Utilities.handleError
                    (\error ->
                        Runner.done Cmd.none
                            |> Runner.logError
                                (case error of
                                    Page.LineNotFound lineNumber ->
                                        String.concat
                                            [ "Line number \u{001B}[1m"
                                            , String.fromInt lineNumber
                                            , "\u{001B}[22m is not in the page for \u{001B}[1m"
                                            , Path.toTitle path
                                            , "\u{001B}[0m"
                                            ]

                                    Page.InvalidOperation errorMessage ->
                                        String.concat
                                            [ "Invalid operation on \u{001B}[1m"
                                            , Path.toTitle path
                                            , "\u{001B}[22m: "
                                            , errorMessage
                                            , "\u{001B}[1m"
                                            ]
                                )
                    )
            )


step2 : Path -> Page -> Runner
step2 path page =
    Runner.run
        |> handlePageSave
            (Runner.done <| Runner.putPage path (Page.clip 2 page))
