port module Main exposing (main)

import Bullet exposing (Bullet(..))
import Command exposing (Command(..))
import Command.Path as Path exposing (Path(..))
import FS
import Flags exposing (Flags)
import Json.Decode as Jd
import Json.Encode exposing (Value)
import Page exposing (Page)
import Utilities exposing (Time, applySecond, handleError, splat2, splat3)


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
    Flags.decode flags
        |> Result.mapError Jd.errorToString
        |> Result.map initialModel
        |> attachCmd


initialModel : ( Time, Command ) -> ModelRecord
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
                    |> Maybe.map (Path.toString time >> fullPath >> FS.read)
                    |> Maybe.withDefault Cmd.none

            Ok ( PutPage page, View path, time ) ->
                Cmd.batch
                    [ put <| Page.terminalOutput page
                    , put <| title (Just (Path.toString time path))
                    ]

            Ok ( PutPage page, command, time ) ->
                page
                    |> Page.clip 2
                    |> Page.terminalOutput
                    |> put
                    |> List.singleton
                    |> (::)
                        (Command.path command
                            |> Maybe.map (Path.toString time)
                            |> title
                            |> put
                        )
                    |> List.reverse
                    |> Cmd.batch

            Ok ( SavePage page, command, time ) ->
                ( command, page )
                    |> Tuple.mapFirst Command.path
                    |> Tuple.mapFirst (Maybe.map (Path.toString time))
                    |> Tuple.mapFirst (Maybe.map fullPath)
                    |> Tuple.mapFirst (Maybe.map FS.write)
                    |> Tuple.mapFirst (Maybe.withDefault (always Cmd.none))
                    |> Tuple.mapSecond Page.toString
                    |> applySecond

            Err error ->
                put error


title : Maybe String -> String
title path =
    case path of
        Just p ->
            "\n Shell Journal — " ++ p

        Nothing ->
            "\n Shell Journal"


fullPath : String -> String
fullPath path =
    String.concat [ ".shjo/", path, ".shjo" ]


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
