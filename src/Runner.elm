port module Runner exposing (Msg, Runner, Update, alsoDo, done, handlePageLoad, handlePageNotFound, handlePageSave, loadPageThen, loadPath, log, onPageLoad, put, putPage, run, savePageThen, subscriptions, update)

import Command.Path as Path exposing (Path)
import FS
import Json.Decode as Jd
import Page exposing (Page)
import Parser
import Result
import Utilities exposing (Time)


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
    | SavedPage


type Update
    = Update Runner (Sub Msg)
    | Done


loadPath : Time -> Path -> Cmd Msg
loadPath time =
    Path.toString time >> fullPath >> FS.read



-- Stepping helpers


loadPageThen : Time -> Path -> Runner -> ( Update, Cmd Msg )
loadPageThen time path thenDo =
    ( Update thenDo onPageLoad
    , loadPath time path
    )


savePageThen : Time -> Path -> Page -> Runner -> ( Update, Cmd Msg )
savePageThen time path page runner =
    ( Update runner onPageSave
    , FS.write (Path.toString time path |> fullPath) (Page.toString page)
    )



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
            ( Done, put <| errorMessage error )

        Ok _ ->
            ( Done, put "Invalid state. This is a bug!" )


errorMessage : Error -> String
errorMessage error =
    case error of
        FilesystemError (FS.NotFound path) ->
            "There isn't a file called " ++ path ++ " in the Shell Journal folder"

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


fullPath : String -> String
fullPath path =
    String.concat [ ".shjo/", path, ".shjo" ]



-- Output helpers


putPage : Path -> Page -> Cmd msg
putPage path page =
    Cmd.batch
        [ put <| Page.terminalOutput page
        , put <| title <| Path.toTitle path
        ]


title : String -> String
title path =
    "\n Shell Journal — \u{001B}[36m" ++ path ++ "\u{001B}[0m"


alsoDo : Cmd Msg -> ( Update, Cmd Msg ) -> ( Update, Cmd Msg )
alsoDo cmd =
    Tuple.mapSecond List.singleton
        >> Tuple.mapSecond ((::) cmd)
        >> Tuple.mapSecond Cmd.batch


log : String -> ( Update, Cmd Msg ) -> ( Update, Cmd Msg )
log =
    put >> alsoDo


done : Cmd Msg -> ( Update, Cmd Msg )
done =
    Tuple.pair Done



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
