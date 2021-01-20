port module Main exposing (main)

import Bullet exposing (Bullet(..))
import Command exposing (Command(..))
import Command.Path exposing (Path(..))
import FS
import Json.Decode exposing (decodeValue, list, string)
import Json.Encode exposing (Value)
import Page exposing (Page)
import Utilities exposing (applySecond, handleError)


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
        |> Result.map GetPage
        |> handleError Error
        |> attachCmd


attachCmd : Model -> ( Model, Cmd Msg )
attachCmd model =
    Tuple.pair model <|
        case model of
            GetPage command ->
                command
                    |> Command.path
                    |> Maybe.map (path >> FS.read)
                    |> Maybe.withDefault Cmd.none

            PutPage (View _) page ->
                put <| Page.terminalOutput page

            PutPage _ page ->
                page
                    |> Page.clip 2
                    |> Page.terminalOutput
                    |> put

            SavePage command page ->
                ( command, page )
                    |> Tuple.mapFirst Command.path
                    |> Tuple.mapFirst (Maybe.map path)
                    |> Tuple.mapFirst (Maybe.map FS.write)
                    |> Tuple.mapFirst (Maybe.withDefault (always Cmd.none))
                    |> Tuple.mapSecond Page.toString
                    |> applySecond

            Error error ->
                put error


arguments : Flags -> String
arguments =
    decodeValue (list string)
        >> Result.withDefault []
        >> String.join " "


path : Path -> String
path path_ =
    pathString <|
        case path_ of
            Today ->
                "today"

            Tomorrow ->
                "tomorrow"

            Yesterday ->
                "yesterday"

            Project p ->
                p


pathString : String -> String
pathString string =
    ".shjo/" ++ string ++ ".shjo"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( GetPage ((View _) as command), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> PutPage command
                |> attachCmd

        ( GetPage ((Add _ bullet content) as command), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.add bullet content
                |> SavePage command
                |> attachCmd

        ( GetPage ((Check _ lineNumber) as command), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.check lineNumber
                |> SavePage command
                |> attachCmd

        ( GetPage ((Star _ lineNumber) as command), GotPage response ) ->
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
