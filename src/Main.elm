port module Main exposing (main)

import Dict exposing (update)
import Json.Decode exposing (decodeValue, list, string)
import Json.Encode exposing (Value)


port put : String -> Cmd msg


port fs : FSRequest -> Cmd msg


port read : (String -> msg) -> Sub msg


type alias FSRequest =
    { method : String
    , data : String
    , path : String
    }


type alias Model =
    ()


type alias Flags =
    Value


type Msg
    = Read String


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
        |> mapCommand
        |> Tuple.pair ()


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


path : String
path =
    "~/.shjo/today.shjo"


mapCommand : Maybe Command -> Cmd Msg
mapCommand command =
    case command of
        Nothing ->
            Cmd.none

        Just View ->
            fs
                { path = path
                , method = "read"
                , data = ""
                }

        Just (Add entry contents) ->
            fs
                { path = path
                , method = "append"
                , data = appendString entry contents
                }


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
update msg () =
    case msg of
        Read data ->
            ( (), put data )


subscriptions : Model -> Sub Msg
subscriptions _ =
    read Read


type Command
    = View
    | Add Entry String


type Entry
    = Task
    | Event
    | Note


appendString : Entry -> String -> String
appendString entry content =
    symbolFor entry ++ " " ++ content ++ "\n"


symbolFor : Entry -> String
symbolFor entry =
    case entry of
        Task ->
            "·"

        Event ->
            "○"

        Note ->
            " "
