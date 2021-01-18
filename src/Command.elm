module Command exposing (Command(..), decode)

import Entry exposing (Entry(..))


type Command
    = View
    | Add Entry String
    | Check Int
    | Star Int
    | WeirdCommand


decode : List String -> Command
decode args =
    Maybe.withDefault WeirdCommand <|
        case args of
            [] ->
                Just View

            "view" :: _ ->
                Just View

            "add" :: rest ->
                decodeAdd rest

            "check" :: rest ->
                decodeCheck rest

            "star" :: rest ->
                decodeStar rest

            _ ->
                Nothing


decodeAdd : List String -> Maybe Command
decodeAdd args =
    let
        entry =
            case List.head args of
                Just "task" ->
                    Just (Task False)

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


decodeCheck : List String -> Maybe Command
decodeCheck args =
    args
        |> List.head
        |> Maybe.andThen String.toInt
        |> Maybe.map Check


decodeStar : List String -> Maybe Command
decodeStar args =
    args
        |> List.head
        |> Maybe.andThen String.toInt
        |> Maybe.map Star


nonEmpty : List a -> Maybe (List a)
nonEmpty list =
    case list of
        [] ->
            Nothing

        _ ->
            Just list
