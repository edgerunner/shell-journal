module Command exposing (Command(..), parse)

import Bullet exposing (Bullet(..))
import Parser as P exposing ((|.), (|=), Parser)


type Command
    = View
    | Add Bullet String
    | Check Int
    | Star Int
    | WeirdCommand


parse : String -> Command
parse =
    P.run commandParser
        >> Result.withDefault WeirdCommand


commandParser : Parser Command
commandParser =
    P.oneOf
        [ P.succeed Add
            |. P.oneOf [ P.keyword "add", P.succeed () ]
            |. P.spaces
            |= bulletParser
            |. P.spaces
            |= P.getChompedString (P.chompWhile (always True))
        , P.succeed Check
            |. P.keyword "check"
            |. P.spaces
            |= P.int
        , P.succeed Star
            |. P.keyword "star"
            |. P.spaces
            |= P.int
        , P.succeed View
            |. P.keyword "view"
        , P.succeed View
            |. P.end
        ]


bulletParser : Parser Bullet
bulletParser =
    P.oneOf
        [ P.succeed (Task False) |. P.keyword "task"
        , P.succeed Event |. P.keyword "event"
        , P.succeed Note |. P.keyword "note"
        ]
