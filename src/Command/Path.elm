module Command.Path exposing (Path(..), parser)

import Parser as P exposing ((|.), (|=), Parser)
import Set


type Path
    = Today
    | Tomorrow
    | Yesterday
    | Project String


parser : Parser Path
parser =
    P.oneOf
        [ P.succeed identity
            |. P.symbol "@"
            |= P.oneOf
                [ pathKeywordParser
                , pathForProjectParser
                , P.problem "this path does not make sense"
                ]
        , P.succeed Today
        ]


pathKeywordParser : Parser Path
pathKeywordParser =
    P.oneOf
        [ P.succeed Today |. P.token "today"
        , P.succeed Tomorrow |. P.token "tomorrow"
        , P.succeed Yesterday |. P.token "yesterday"
        ]


pathForProjectParser : Parser Path
pathForProjectParser =
    P.variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }
        |> P.map Project
