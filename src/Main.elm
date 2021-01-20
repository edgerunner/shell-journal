port module Main exposing (main)

import Bullet exposing (Bullet(..))
import Command exposing (Command(..))
import FS
import Json.Decode exposing (decodeValue, list, string)
import Json.Encode exposing (Value)
import Page exposing (Page)


port put : String -> Cmd msg


type Model
    = GetPage Command
    | PutPage Command Page
    | SavePage Command Page
    | Error String


type alias Flags =
    Value


type Msg
    = GotPage Value
    | SavedPage


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    arguments flags
        |> Command.parse
        |> Result.map (Tuple.second >> GetPage)
        |> handleError Error
        |> attachCmd


handleError : (err -> ok) -> Result err ok -> ok
handleError handle result =
    case result of
        Err err ->
            handle err

        Ok ok ->
            ok


attachCmd : Model -> ( Model, Cmd Msg )
attachCmd model =
    Tuple.pair model <|
        case model of
            GetPage _ ->
                FS.read path

            PutPage View page ->
                put <| Page.terminalOutput page

            PutPage _ page ->
                page
                    |> Page.clip 2
                    |> Page.terminalOutput
                    |> put

            SavePage _ page ->
                FS.write path <| Page.toString page

            Error error ->
                put error


arguments : Flags -> String
arguments =
    decodeValue (list string)
        >> Result.withDefault []
        >> String.join " "


path : String
path =
    ".shjo/today.shjo"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( GetPage View, GotPage response ) ->
            response
                |> decodeAndParsePage
                |> PutPage View
                |> attachCmd

        ( GetPage ((Add bullet content) as command), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.add bullet content
                |> SavePage command
                |> attachCmd

        ( GetPage ((Check lineNumber) as command), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.check lineNumber
                |> SavePage command
                |> attachCmd

        ( GetPage ((Star lineNumber) as command), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.star lineNumber
                |> SavePage command
                |> attachCmd

        ( SavePage command page, SavedPage ) ->
            page
                |> PutPage command
                |> attachCmd

        _ ->
            ( model, put "Error: invalid transition" )


decodeAndParsePage : Value -> Page
decodeAndParsePage response =
    response
        |> decodeValue string
        |> Result.withDefault "Parse error"
        |> Page.parse
        |> Result.withDefault []


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        GetPage _ ->
            FS.subscription GotPage

        SavePage _ _ ->
            FS.subscription (always SavedPage)

        PutPage _ _ ->
            Sub.none

        Error _ ->
            Sub.none
