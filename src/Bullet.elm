module Bullet exposing (Bullet(..), symbol)


type Bullet
    = Task Bool
    | Event
    | Note


symbol : Bullet -> String
symbol bullet =
    case bullet of
        Task False ->
            "·"

        Task True ->
            "╳"

        Event ->
            "○"

        Note ->
            " "
