port module Main exposing (main)

import Dict exposing (update)
import Json.Decode exposing (decodeValue, list, string)
import Json.Encode exposing (Value)


port put : String -> Cmd msg


type alias Model =
    ()


type alias Flags =
    Value


type alias Msg =
    ()


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
        |> decodeCommand
        |> Maybe.map (putAll [ Debug.toString, outputString ])
        |> Maybe.withDefault Cmd.none
        |> Tuple.pair ()


putAll : List (Command -> String) -> Command -> Cmd Msg
putAll transforms command =
    transforms
        |> List.map (put << (|>) command)
        |> Cmd.batch


arguments : Flags -> List String
arguments =
    Result.withDefault []
        << decodeValue (list string)


decodeCommand : List String -> Maybe Command
decodeCommand args =
    case args of
        [ "view" ] ->
            Just View

        "add" :: rest ->
            decodeAdd rest

        _ ->
            Nothing


decodeAdd : List String -> Maybe Command
decodeAdd args =
    let
        entry =
            case List.head args of
                Just "task" ->
                    Just Task

                Just "event" ->
                    Just Event

                Just "note" ->
                    Just Note

                _ ->
                    Nothing

        contents =
            List.tail args
                |> Maybe.andThen nonEmpty
                |> Maybe.map (String.join " ")
    in
    Maybe.map2 Add entry contents


nonEmpty : List a -> Maybe (List a)
nonEmpty list =
    case list of
        [] ->
            Nothing

        _ ->
            Just list


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Command
    = View
    | Add Entry String


type Entry
    = Task
    | Event
    | Note


outputString : Command -> String
outputString command =
    case command of
        Add entry content ->
            symbolFor entry ++ " " ++ content

        _ ->
            ""


symbolFor : Entry -> String
symbolFor entry =
    case entry of
        Task ->
            "·"

        Event ->
            "○"

        Note ->
            " "
