port module Runner exposing
    ( Msg
    , Runner
    , Update
    , alsoDo
    , done
    , doneWith
    , fail
    , handleGotPageList
    , handlePageLoad
    , handlePageNotFound
    , handlePageSave
    , init
    , loadPageThen
    , log
    , logError
    , onPageList
    , onPageLoad
    , put
    , putPage
    , run
    , savePageThen
    , subscriptions
    , update
    )

import Command.Path as Path exposing (Path)
import FS
import Flags exposing (Flags)
import Json.Decode as Jd
import Page exposing (Page)
import Parser
import Result
import Style
import Utilities


port put : String -> Cmd msg


type alias Msg =
    Result Error Success


type alias Runner =
    Msg -> ( Update, Cmd Msg )


type Error
    = FilesystemError FS.Error
    | ParsingError (List Parser.DeadEnd)
    | DecodingError Jd.Error


type Success
    = LoadedPage Page
    | GotPageList (List String)
    | SavedPage


type Update
    = Update Runner (Sub Msg)
    | Done



-- Stepping helpers


loadPageThen : Flags -> Path -> Runner -> ( Update, Cmd Msg )
loadPageThen flags path runner =
    init runner onPageLoad (FS.read (Path.toFSPath flags path))


savePageThen : Flags -> Path -> Page -> Runner -> ( Update, Cmd Msg )
savePageThen flags path page runner =
    init runner
        onPageSave
        (FS.write (Path.toFSPath flags path) (Page.toString flags page))



-- Response unwrappers


handlePageLoad : (Page -> ( Update, Cmd Msg )) -> Runner -> Runner
handlePageLoad handler next msg =
    case msg of
        Ok (LoadedPage page) ->
            handler page

        _ ->
            next msg


handlePageSave : ( Update, Cmd Msg ) -> Runner -> Runner
handlePageSave handler next msg =
    case msg of
        Ok SavedPage ->
            handler

        _ ->
            next msg


handleGotPageList : (List String -> ( Update, Cmd Msg )) -> Runner -> Runner
handleGotPageList handler next msg =
    case msg of
        Ok (GotPageList list) ->
            handler list

        _ ->
            next msg


handlePageNotFound : (String -> ( Update, Cmd Msg )) -> Runner -> Runner
handlePageNotFound handler next msg =
    case msg of
        Err (FilesystemError (FS.NotFound path)) ->
            handler path

        _ ->
            next msg


run : Runner
run msg =
    case msg of
        Err error ->
            logError (errorMessage error) (done 1)

        Ok _ ->
            logError "Invalid state. This is a bug!" (done 2)


init : Runner -> Sub Msg -> Cmd Msg -> ( Update, Cmd Msg )
init runner sub cmd =
    ( Update runner sub, cmd )


errorMessage : Error -> String
errorMessage error =
    case error of
        FilesystemError (FS.NotFound path) ->
            "There isn't a file at " ++ path

        FilesystemError _ ->
            "Could not access that file"

        ParsingError _ ->
            "Error while parsing the page"

        DecodingError _ ->
            "Error decoding data"



-- Subscription handlers


onPageLoad : Sub Msg
onPageLoad =
    Result.mapError FilesystemError
        >> Result.andThen (Jd.decodeValue Jd.string >> Result.mapError DecodingError)
        >> Result.andThen (Page.parse >> Result.mapError ParsingError)
        >> Result.map LoadedPage
        |> FS.subscription


onPageSave : Sub Msg
onPageSave =
    Result.mapError FilesystemError
        >> Result.map (always SavedPage)
        |> FS.subscription


onPageList : Sub Msg
onPageList =
    Result.mapError FilesystemError
        >> Result.andThen (Jd.decodeValue (Jd.list Jd.string) >> Result.mapError DecodingError)
        >> Result.map GotPageList
        |> FS.subscription



-- Output helpers


putPage : Flags -> Path -> Page -> Cmd msg
putPage flags path page =
    Cmd.batch
        [ put <| Page.terminalOutput flags page
        , put <| title <| Path.toTitle flags path
        ]


title : String -> String
title path =
    String.concat
        [ "\n Shell Journal — "
        , Style.escape [ Style.cyan ]
        , path
        , Style.escape [ Style.reset ]
        ]


alsoDo : Cmd Msg -> ( Update, Cmd Msg ) -> ( Update, Cmd Msg )
alsoDo cmd =
    Tuple.mapSecond List.singleton
        >> Tuple.mapSecond ((::) cmd)
        >> Tuple.mapSecond Cmd.batch


log : String -> ( Update, Cmd Msg ) -> ( Update, Cmd Msg )
log =
    put >> alsoDo


logError : String -> ( Update, Cmd Msg ) -> ( Update, Cmd Msg )
logError =
    String.append (Style.escape [ Style.red ])
        >> String.append
        >> (|>) (Style.escape [ Style.reset ])
        >> log


doneWith : Cmd Msg -> ( Update, Cmd Msg )
doneWith =
    Tuple.pair Done


port exit : Int -> Cmd msg


done : Int -> ( Update, Cmd Msg )
done exitCode =
    ( Done, exit exitCode )


fail : (error -> String) -> Result error ( Update, Cmd Msg ) -> ( Update, Cmd Msg )
fail errorToMessage =
    Utilities.handleError (errorToMessage >> logError >> (|>) (done 1))



-- TEA functions for Program


update : Msg -> Update -> ( Update, Cmd Msg )
update msg update_ =
    case update_ of
        Update updater _ ->
            updater msg

        Done ->
            ( Done, Cmd.none )


subscriptions : Update -> Sub Msg
subscriptions update_ =
    case update_ of
        Update _ sub ->
            sub

        Done ->
            Sub.none
