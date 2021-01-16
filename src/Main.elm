port module Main exposing (main)

import Command exposing (Command(..))
import Entry exposing (Entry(..))
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
    | AddEntry Entry String Value
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
                        ( Just <| AddEntry entry contents, FS.read path )

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

        AddEntry entry content response ->
            let
                page =
                    response
                        |> decodeValue string
                        |> Result.withDefault "Parse error"
                        |> Page.parse
                        |> Result.withDefault []
                        |> Page.add entry content

                writeToTerminal =
                    page
                        |> Page.terminalOutput
                        |> put

                writeToFile =
                    page
                        |> Page.toString
                        |> FS.write path
            in
            ( Nothing, Cmd.batch [ writeToTerminal, writeToFile ] )

        CheckTask lineNumber response ->
            let
                page =
                    response
                        |> decodeValue string
                        |> Result.withDefault "Parse error"
                        |> Page.parse
                        |> Result.withDefault []
                        |> Page.check lineNumber

                writeToTerminal =
                    page
                        |> Page.terminalOutput
                        |> put

                writeToFile =
                    page
                        |> Page.toString
                        |> FS.write path
            in
            ( Nothing, Cmd.batch [ writeToTerminal, writeToFile ] )


subscriptions : Model -> Sub Msg
subscriptions model =
    model
        |> Maybe.map FS.subscription
        |> Maybe.withDefault Sub.none
