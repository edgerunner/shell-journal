port module Main exposing (main)

import Command exposing (Command(..), Entry(..))
import FS
import Json.Decode exposing (decodeValue, list, string)
import Json.Encode exposing (Value)
import Page


port put : String -> Cmd msg


type alias Model =
    Maybe (Value -> Msg)


type alias Flags =
    Value


type Msg
    = ViewFile Value
    | CheckTask Int Value


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
        |> mapCommand


arguments : Flags -> List String
arguments =
    Result.withDefault []
        << decodeValue (list string)


path : String
path =
    ".shjo/today.shjo"


mapCommand : Maybe Command -> ( Model, Cmd Msg )
mapCommand maybeCommand =
    maybeCommand
        |> Maybe.map
            (\command ->
                case command of
                    View ->
                        ( Just ViewFile, FS.read path )

                    Add entry contents ->
                        Page.lineToString ( entry, contents )
                            |> FS.append path
                            |> Tuple.pair Nothing

                    Check lineNumber ->
                        ( Just <| CheckTask lineNumber, FS.read path )
            )
        |> Maybe.withDefault ( Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        ViewFile response ->
            response
                |> decodeValue string
                |> Result.withDefault "Parse error"
                |> Page.parse
                |> Result.withDefault []
                |> Page.terminalOutput
                |> put
                |> Tuple.pair Nothing

        CheckTask lineNumber response ->
            response
                |> decodeValue string
                |> Result.withDefault "Parse error"
                |> Page.parse
                |> Result.withDefault []
                |> Page.check lineNumber
                |> Page.toString
                |> FS.write path
                |> Tuple.pair Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    model
        |> Maybe.map FS.subscription
        |> Maybe.withDefault Sub.none
