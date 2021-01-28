port module Runner exposing (Error(..), Msg, Success(..), Update(..), handlePageLoad, handlePageSave, loadPageThen, loadPath, onPageLoad, put, savePageThen)

import Command.Path as Path exposing (Path)
import FS
import Json.Decode as Jd exposing (Value)
import Page exposing (Page)
import Parser
import Result
import Utilities exposing (Time)


port put : String -> Cmd msg


type alias Msg =
    Result Error Success


type Error
    = FilesystemError Value
    | ParsingError (List Parser.DeadEnd)
    | DecodingError Jd.Error


type Success
    = LoadedPage Page
    | SavedPage


type Update
    = Update (Msg -> ( Update, Cmd Msg )) (Sub Msg)
    | Done


loadPath : Time -> Path -> Cmd Msg
loadPath time =
    Path.toString time >> fullPath >> FS.read



-- Stepping helpers


loadPageThen : Time -> Path -> (Msg -> ( Update, Cmd Msg )) -> ( Update, Cmd Msg )
loadPageThen time path thenDo =
    ( Update thenDo onPageLoad
    , loadPath time path
    )


savePageThen : Time -> Path -> Page -> (Msg -> ( Update, Cmd Msg )) -> ( Update, Cmd Msg )
savePageThen time path page thenDo =
    ( Update thenDo onPageSave
    , FS.write (Path.toString time path |> fullPath) (Page.toString page)
    )



-- Response unwrappers


handlePageLoad : (Page -> ( Update, Cmd Msg )) -> Msg -> ( Update, Cmd Msg )
handlePageLoad handler msg =
    case msg of
        Ok (LoadedPage page) ->
            handler page

        Err error ->
            ( Done, put <| errorMessage error )

        Ok _ ->
            ( Done, put "Invalid state. This is a bug!" )


handlePageSave : ( Update, Cmd Msg ) -> Msg -> ( Update, Cmd Msg )
handlePageSave handler msg =
    case msg of
        Ok SavedPage ->
            handler

        Err error ->
            ( Done, put <| errorMessage error )

        Ok _ ->
            ( Done, put "Invalid state. This is a bug!" )


errorMessage : Error -> String
errorMessage error =
    case error of
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
