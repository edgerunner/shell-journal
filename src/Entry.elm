module Entry exposing (Entry(..), symbol)


type Entry
    = Task Bool
    | Event
    | Note


symbol : Entry -> String
symbol entry =
    case entry of
        Task False ->
            "·"

        Task True ->
            "╳"

        Event ->
            "○"

        Note ->
            " "
