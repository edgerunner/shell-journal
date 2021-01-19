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

            PutPage View page ->
                put <| Page.terminalOutput page

            PutPage _ page ->
                page
                    |> Page.clip 2
                    |> Page.terminalOutput
                    |> put

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

        ( GetPage (Add bullet content), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.add bullet content
                |> SavePage (Add bullet content)
                |> attachCmd

        ( GetPage (Check lineNumber), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.check lineNumber
                |> SavePage (Check lineNumber)
                |> attachCmd

        ( GetPage (Star lineNumber), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.star lineNumber
                |> SavePage (Star lineNumber)
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
