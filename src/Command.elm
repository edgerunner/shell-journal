module Command exposing (Command(..), decode)

import Bullet exposing (Bullet(..))


type Command
    = View
    | Add Bullet String
    | Check Int
    | Star Int
    | WeirdCommand


decode : List String -> Command
decode args =
    case args of
        [] ->
            View

        "view" :: _ ->
            View

        "add" :: bullet :: rest ->
            decodeAdd bullet rest

        "check" :: rest ->
            decodeByLineNumber Check rest

        "star" :: rest ->
            decodeByLineNumber Star rest

        bullet :: rest ->
            decodeAdd bullet rest


decodeAdd : String -> List String -> Command
decodeAdd bulletCommand args =
    let
        bullet =
            case bulletCommand of
                "task" ->
                    Just (Task False)

                "event" ->
                    Just Event

                "note" ->
                    Just Note

                _ ->
                    Nothing

        contents =
            args
                |> nonEmpty
                |> Maybe.map (String.join " ")
    in
    Maybe.map2 Add bullet contents
        |> Maybe.withDefault WeirdCommand


decodeByLineNumber : (Int -> Command) -> List String -> Command
decodeByLineNumber command args =
    args
        |> List.head
        |> Maybe.andThen String.toInt
        |> Maybe.map command
        |> Maybe.withDefault WeirdCommand


nonEmpty : List a -> Maybe (List a)
nonEmpty list =
    case list of
        [] ->
            Nothing

        _ ->
            Just list
