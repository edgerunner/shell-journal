module Command exposing (Command(..), parse)

import Bullet exposing (Bullet(..))
import Command.Path as Path exposing (Path)
import Parser as P exposing ((|.), (|=), Parser)


type Command
    = View
    | Add Bullet String
    | Check Int
    | Star Int


parse : String -> Result String ( Path, Command )
parse =
    P.run parser
        >> Result.mapError P.deadEndsToString


parser : Parser ( Path, Command )
parser =
    P.succeed Tuple.pair
        |= Path.parser
        |. P.spaces
        |= commandParser


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
