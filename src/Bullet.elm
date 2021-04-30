module Bullet exposing (Bullet(..), TaskState(..), parser, symbol)

import Command.Path as Path exposing (Path)
import Parser exposing ((|.), (|=), Parser)


type Bullet
    = Task TaskState
    | Event
    | Note


type TaskState
    = Pending
    | Done
    | Moved Path Int


symbol : Bullet -> String
symbol bullet =
    case bullet of
        Task Pending ->
            "·"

        Task Done ->
            "╳"

        Task (Moved _ _) ->
            ">"

        Event ->
            "○"

        Note ->
            "-"


parser : Parser Bullet
parser =
    ([ Task Pending, Task Done, Event, Note ]
        |> List.map parserFor
        |> (::) taskMovedParser
        |> Parser.oneOf
    )
        |. Parser.spaces


taskMovedParser : Parser Bullet
taskMovedParser =
    Parser.succeed Moved
        |. Parser.symbol ">"
        |. Parser.symbol " ["
        |= Path.pathParser
        |. Parser.symbol ":"
        |= Parser.int
        |. Parser.symbol "]"
        |> Parser.map Task


parserFor : Bullet -> Parser Bullet
parserFor thisBullet =
    Parser.succeed thisBullet
        |. Parser.symbol (symbol thisBullet)
