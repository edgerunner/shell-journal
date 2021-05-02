module Command exposing (Command(..), parse, path)

import Bullet exposing (Bullet(..), TaskState(..))
import Command.Path as Path exposing (Path)
import Parser as P exposing ((|.), (|=), Parser)


type Command
    = View Path
    | Add Path Bullet String
    | Check Path Int
    | Star Path Int
    | Strike Path Int
    | Move Path Int Path
    | List Path
    | Follow Path Int


parse : Maybe String -> String -> Result String Command
parse defaultPath =
    (P.run <| parser defaultPath)
        >> Result.mapError P.deadEndsToString


parser : Maybe String -> Parser Command
parser defaultPath =
    Path.parser defaultPath
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
        , P.succeed (Strike path_)
            |. P.keyword "strike"
            |. P.spaces
            |= P.int
        , P.succeed (Follow path_)
            |. P.keyword "follow"
            |. P.spaces
            |= P.int
        , P.succeed (Move path_)
            |. P.keyword "move"
            |. P.spaces
            |= P.int
            |. P.spaces
            |= Path.parser Nothing
        , P.succeed (List path_)
            |. P.keyword "list"
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


path : Command -> Path
path command =
    case command of
        View path_ ->
            path_

        Add path_ _ _ ->
            path_

        Star path_ _ ->
            path_

        Strike path_ _ ->
            path_

        Check path_ _ ->
            path_

        Move path_ _ _ ->
            path_

        List path_ ->
            path_

        Follow path_ _ ->
            path_
