port module Main exposing (main)

import Bullet exposing (Bullet(..))
import Command exposing (Command(..))
import Command.Path exposing (Path(..))
import FS
import Json.Decode as Jd
import Json.Encode exposing (Value)
import Page exposing (Page)
import Time exposing (Posix, Zone)
import Tuple
import Utilities exposing (applySecond, handleError, splat2, splat3)


port put : String -> Cmd msg


type Phase
    = GetPage
    | PutPage Page
    | SavePage Page


type alias Model =
    Result String ModelRecord


type alias ModelRecord =
    { time : Time
    , command : Command
    , phase : Phase
    }


type alias Time =
    ( Posix, Zone )


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
        |> Result.mapError Jd.errorToString
        |> Result.map initialModel
        |> attachCmd


initialModel : ( ( Posix, Zone ), Command ) -> ModelRecord
initialModel ( time, command ) =
    { time = time
    , command = command
    , phase = GetPage
    }


attachCmd : Model -> ( Model, Cmd Msg )
attachCmd model =
    Tuple.pair model <|
        case Result.map (splat3 .phase .command .time) model of
            Ok ( GetPage, command, time ) ->
                command
                    |> Command.path
                    |> Maybe.map (path time >> FS.read)
                    |> Maybe.withDefault Cmd.none

            Ok ( PutPage page, View _, _ ) ->
                put <| Page.terminalOutput page

            Ok ( PutPage page, _, _ ) ->
                page
                    |> Page.clip 2
                    |> Page.terminalOutput
                    |> put

            Ok ( SavePage page, command, time ) ->
                ( command, page )
                    |> Tuple.mapFirst Command.path
                    |> Tuple.mapFirst (Maybe.map (path time))
                    |> Tuple.mapFirst (Maybe.map FS.write)
                    |> Tuple.mapFirst (Maybe.withDefault (always Cmd.none))
                    |> Tuple.mapSecond Page.toString
                    |> applySecond

            Err error ->
                put error


decode : Flags -> Result Jd.Error ( Time, Command )
decode =
    Jd.decodeValue flagsDecoder


flagsDecoder : Jd.Decoder ( Time, Command )
flagsDecoder =
    Jd.map2 Tuple.pair timezoneDecoder commandDecoder


timezoneDecoder : Jd.Decoder Time
timezoneDecoder =
    Jd.map2 Tuple.pair timeDecoder zoneDecoder


timeDecoder : Jd.Decoder Posix
timeDecoder =
    Jd.field "time" Jd.int
        |> Jd.map Time.millisToPosix


zoneDecoder : Jd.Decoder Zone
zoneDecoder =
    Jd.field "zone" Jd.int
        |> Jd.map Time.customZone
        |> Jd.map ((|>) [])


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


path : Time -> Path -> String
path time path_ =
    pathString <|
        case path_ of
            Today ->
                datePath time

            Tomorrow ->
                datePath (shiftDays 1 time)

            Yesterday ->
                datePath (shiftDays -1 time)

            Project p ->
                p


datePath : Time -> String
datePath ( posix, zone ) =
    let
        year =
            Time.toYear zone posix |> String.fromInt

        day =
            Time.toDay zone posix |> String.fromInt

        month =
            case Time.toMonth zone posix of
                Time.Jan ->
                    "01"

                Time.Feb ->
                    "02"

                Time.Mar ->
                    "03"

                Time.Apr ->
                    "04"

                Time.May ->
                    "05"

                Time.Jun ->
                    "06"

                Time.Jul ->
                    "07"

                Time.Aug ->
                    "08"

                Time.Sep ->
                    "09"

                Time.Oct ->
                    "10"

                Time.Nov ->
                    "11"

                Time.Dec ->
                    "12"
    in
    String.join "-" [ year, month, day ]


shiftDays : Int -> Time -> Time
shiftDays shift =
    Tuple.mapFirst
        (Time.posixToMillis
            >> (+) (shift * 86400000)
            >> Time.millisToPosix
        )


pathString : String -> String
pathString string =
    ".shjo/" ++ string ++ ".shjo"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( Result.map (splat2 .phase .command) model, msg ) of
        ( Ok ( GetPage, View _ ), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> PutPage
                |> nextPhase model
                |> attachCmd

        ( Ok ( GetPage, Add _ bullet content ), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.add bullet content
                |> SavePage
                |> nextPhase model
                |> attachCmd

        ( Ok ( GetPage, Check _ lineNumber ), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.check lineNumber
                |> SavePage
                |> nextPhase model
                |> attachCmd

        ( Ok ( GetPage, Star _ lineNumber ), GotPage response ) ->
            response
                |> decodeAndParsePage
                |> Page.star lineNumber
                |> SavePage
                |> nextPhase model
                |> attachCmd

        ( Ok ( GetPage, Add _ bullet content ), FSError _ ) ->
            Page.blank
                |> Page.add bullet content
                |> SavePage
                |> nextPhase model
                |> attachCmd

        ( Ok ( GetPage, _ ), FSError _ ) ->
            Err "Could not get that page"
                |> attachCmd

        ( Ok ( SavePage page, _ ), SavedPage ) ->
            page
                |> PutPage
                |> nextPhase model
                |> attachCmd

        ( Ok ( SavePage _, _ ), FSError _ ) ->
            Err "Could not save that page"
                |> attachCmd

        _ ->
            Err "Invalid transition"
                |> attachCmd


nextPhase : Model -> Phase -> Model
nextPhase model phase =
    Result.map (\rec -> { rec | phase = phase }) model


decodeAndParsePage : Value -> Page
decodeAndParsePage response =
    response
        |> Jd.decodeValue Jd.string
        |> Result.withDefault "Parse error"
        |> Page.parse
        |> Result.withDefault []


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Ok rec ->
            case rec.phase of
                GetPage ->
                    FS.subscription (resultToMsg GotPage)

                SavePage _ ->
                    FS.subscription (resultToMsg (always SavedPage))

                PutPage _ ->
                    Sub.none

        Err _ ->
            Sub.none


resultToMsg : (Value -> Msg) -> Result Value Value -> Msg
resultToMsg constructor =
    Result.map constructor
        >> handleError FSError
