port module Main exposing (main)

import Command exposing (Command(..))
import Entry exposing (Entry(..))
import FS
import Json.Decode exposing (decodeValue, list, string)
import Json.Encode exposing (Value)
import Page exposing (Page)


port put : String -> Cmd msg


type alias Model =
    Maybe (Value -> Msg)


type alias Flags =
    Value


type Msg
    = Resolve Command Value


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
        |> Maybe.map (Resolve >> Just)
        |> Maybe.map Tuple.pair
        |> Maybe.map ((|>) (FS.read path))
        |> Maybe.withDefault ( Nothing, Cmd.none )


arguments : Flags -> List String
arguments =
    Result.withDefault []
        << decodeValue (list string)


path : String
path =
    ".shjo/today.shjo"


update : Msg -> Model -> ( Model, Cmd Msg )
update (Resolve command response) _ =
    ( Nothing
    , case command of
        View ->
            response
                |> decodeAndParsePage
                |> Page.terminalOutput 0
                |> put

        Add entry content ->
            response
                |> decodeAndParsePage
                |> Page.add entry content
                |> (\page ->
                        saveAndOutput (List.length page) page
                   )

        Check lineNumber ->
            response
                |> decodeAndParsePage
                |> Page.check lineNumber
                |> saveAndOutput lineNumber
    )


saveAndOutput : Int -> Page -> Cmd Msg
saveAndOutput highlight page =
    let
        writeToTerminal =
            page
                |> Page.terminalOutput highlight
                |> put

        writeToFile =
            page
                |> Page.toString
                |> FS.write path
    in
    Cmd.batch [ writeToTerminal, writeToFile ]


decodeAndParsePage : Value -> Page
decodeAndParsePage response =
    response
        |> decodeValue string
        |> Result.withDefault "Parse error"
        |> Page.parse
        |> Result.withDefault []


subscriptions : Model -> Sub Msg
subscriptions model =
    model
        |> Maybe.map FS.subscription
        |> Maybe.withDefault Sub.none
