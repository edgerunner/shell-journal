port module Main exposing (main)

import Bullet exposing (Bullet(..))
import Command exposing (Command(..))
import Command.Path exposing (Path(..))
import FS
import Json.Decode as Jd
import Json.Encode exposing (Value)
import Page exposing (Page)
import Time exposing (Posix)
import Tuple
import Utilities exposing (applySecond, handleError)


port put : String -> Cmd msg


type Phase
    = GetPage
    | PutPage Page
    | SavePage Page
    | Error String


type alias Model =
    { time : Posix
    , command : Command
    , phase : Phase
    }


type alias Flags =
    Value


type Msg
    = GotPage Value
    | SavedPage
    | FSError Value


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    decode flags
        |> buildInitialModel
        |> attachCmd


buildInitialModel : Result Jd.Error ( Posix, Command ) -> Model
buildInitialModel decodeResult =
    case decodeResult of
        Ok ( time, command ) ->
            Model time command GetPage

        Err error ->
            errorModel <| Jd.errorToString error


errorModel : String -> Model
errorModel error =
    { time = Time.millisToPosix 0
    , command = View Today -- horrible horrible data modeling. Fix it
    , phase = Error error
    }


attachCmd : Model -> ( Model, Cmd Msg )
attachCmd ({ phase, command } as model) =
    Tuple.pair model <|
        case ( phase, command ) of
            ( GetPage, _ ) ->
                command
                    |> Command.path
                    |> Maybe.map (path >> FS.read)
                    |> Maybe.withDefault Cmd.none

            ( PutPage page, View _ ) ->
                put <| Page.terminalOutput page

            ( PutPage page, _ ) ->
                page
                    |> Page.clip 2
                    |> Page.terminalOutput
                    |> put

            ( SavePage page, _ ) ->
                ( command, page )
                    |> Tuple.mapFirst Command.path
                    |> Tuple.mapFirst (Maybe.map path)
                    |> Tuple.mapFirst (Maybe.map FS.write)
                    |> Tuple.mapFirst (Maybe.withDefault (always Cmd.none))
                    |> Tuple.mapSecond Page.toString
                    |> applySecond

            ( Error error, _ ) ->
                put error


decode : Flags -> Result Jd.Error ( Posix, Command )
decode =
    Jd.decodeValue flagsDecoder


flagsDecoder : Jd.Decoder ( Posix, Command )
flagsDecoder =
    Jd.map2 Tuple.pair timeDecoder commandDecoder


timeDecoder : Jd.Decoder Posix
timeDecoder =
    Jd.field "time" Jd.int
        |> Jd.map Time.millisToPosix


commandDecoder : Jd.Decoder Command
commandDecoder =
    argsDecoder
        |> Jd.andThen
            (Command.parse
                >> Result.map Jd.succeed
                >> handleError Jd.fail
            )


argsDecoder : Jd.Decoder String
argsDecoder =
    Jd.field "args" (Jd.list Jd.string)
        |> Jd.map (String.join " ")


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
update msg ({ phase, command } as model) =
    case ( phase, command, msg ) of
        ( GetPage, View _, GotPage response ) ->
            response
                |> decodeAndParsePage
                |> PutPage
                |> nextPhase model
                |> attachCmd

        ( GetPage, Add _ bullet content, GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.add bullet content
                |> SavePage
                |> nextPhase model
                |> attachCmd

        ( GetPage, Check _ lineNumber, GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.check lineNumber
                |> SavePage
                |> nextPhase model
                |> attachCmd

        ( GetPage, Star _ lineNumber, GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.star lineNumber
                |> SavePage
                |> nextPhase model
                |> attachCmd

        ( GetPage, Add _ bullet content, FSError _ ) ->
            Page.blank
                |> Page.add bullet content
                |> SavePage
                |> nextPhase model
                |> attachCmd

        ( GetPage, _, FSError _ ) ->
            errorModel "Could not get that page"
                |> attachCmd

        ( SavePage page, _, SavedPage ) ->
            page
                |> PutPage
                |> nextPhase model
                |> attachCmd

        ( SavePage _, _, FSError _ ) ->
            errorModel "Could not save that page"
                |> attachCmd

        _ ->
            errorModel "Invalid transition"
                |> attachCmd


nextPhase : Model -> Phase -> Model
nextPhase model phase =
    { model | phase = phase }


decodeAndParsePage : Value -> Page
decodeAndParsePage response =
    response
        |> Jd.decodeValue Jd.string
        |> Result.withDefault "Parse error"
        |> Page.parse
        |> Result.withDefault []


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.phase of
        GetPage ->
            FS.subscription (resultToMsg GotPage)

        SavePage _ ->
            FS.subscription (resultToMsg (always SavedPage))

        PutPage _ ->
            Sub.none

        Error _ ->
            Sub.none


resultToMsg : (Value -> Msg) -> Result Value Value -> Msg
resultToMsg constructor =
    Result.map constructor
        >> handleError FSError
