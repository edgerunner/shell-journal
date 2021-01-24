module Bullet exposing (Bullet(..), parser, symbol)

import Parser exposing ((|.), Parser)


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
            "-"


parser : Parser Bullet
parser =
    Parser.oneOf (List.map parserFor [ Task True, Task False, Event, Note ])
        |. Parser.spaces


parserFor : Bullet -> Parser Bullet
parserFor thisBullet =
    Parser.succeed thisBullet
        |. Parser.symbol (symbol thisBullet)
