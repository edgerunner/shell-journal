port module Main exposing (main)

import Command exposing (Command(..))
import Entry exposing (Entry(..))
import FS
import Json.Decode exposing (decodeValue, list, string)
import Json.Encode exposing (Value)
import Page exposing (Page)


port put : String -> Cmd msg


type Model
    = GetPage Command
    | PutPage Command Page
    | SavePage Command Page


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
        |> Command.decode
        |> GetPage
        |> attachCmd


attachCmd : Model -> ( Model, Cmd Msg )
attachCmd model =
    Tuple.pair model <|
        case model of
            GetPage WeirdCommand ->
                put "Error: this command does not make sense to me"

            GetPage _ ->
                FS.read path

            PutPage _ page ->
                put <| Page.terminalOutput page

            SavePage _ page ->
                FS.write path <| Page.toString page


arguments : Flags -> List String
arguments =
    Result.withDefault []
        << decodeValue (list string)


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

        ( GetPage (Add entry content), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.add entry content
                |> SavePage (Add entry content)
                |> attachCmd

        ( GetPage (Check lineNumber), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.check lineNumber
                |> SavePage (Check lineNumber)
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
