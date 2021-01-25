module Command exposing (Command(..), parse, path)

import Bullet exposing (Bullet(..), TaskState(..))
import Command.Path as Path exposing (Path)
import Parser as P exposing ((|.), (|=), Parser)


type Command
    = View Path
    | Add Path Bullet String
    | Check Path Int
    | Star Path Int


parse : String -> Result String Command
parse =
    P.run parser
        >> Result.mapError P.deadEndsToString


parser : Parser Command
parser =
    Path.parser
        |. P.spaces
        |> P.andThen commandParser


commandParser : Path -> Parser Command
commandParser path_ =
    P.oneOf
        [ P.succeed (Add path_)
            |. P.oneOf [ P.keyword "add", P.succeed () ]
            |. P.spaces
            |= bulletParser
            |. P.spaces
            |= P.getChompedString (P.chompWhile (always True))
        , P.succeed (Check path_)
            |. P.keyword "check"
            |. P.spaces
            |= P.int
        , P.succeed (Star path_)
            |. P.keyword "star"
            |. P.spaces
            |= P.int
        , P.succeed (View path_)
            |. P.keyword "view"
        , P.succeed (View path_)
            |. P.end
        ]


bulletParser : Parser Bullet
bulletParser =
    P.oneOf
        [ P.succeed (Task Pending) |. P.keyword "task"
        , P.succeed Event |. P.keyword "event"
        , P.succeed Note |. P.keyword "note"
        ]


path : Command -> Maybe Path
path command =
    case command of
        View path_ ->
            Just path_

        Add path_ _ _ ->
            Just path_

        Star path_ _ ->
            Just path_

        Check path_ _ ->
            Just path_
