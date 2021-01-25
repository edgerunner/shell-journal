module Bullet exposing (Bullet(..), TaskState(..), parser, symbol)

import Parser exposing ((|.), (|=), Parser)


type Bullet
    = Task TaskState
    | Event
    | Note


type TaskState
    = Pending
    | Done
    | Moved String


symbol : Bullet -> String
symbol bullet =
    case bullet of
        Task Pending ->
            "·"

        Task Done ->
            "╳"

        Task (Moved _) ->
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
    Parser.succeed (Task << Moved)
        |. Parser.symbol (symbol <| Task <| Moved "")
        |. Parser.symbol " ["
        |= (Parser.chompUntil "]"
                |> Parser.getChompedString
           )


parserFor : Bullet -> Parser Bullet
parserFor thisBullet =
    Parser.succeed thisBullet
        |. Parser.symbol (symbol thisBullet)
