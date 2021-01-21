module Command.Path exposing (Path(..), parser, toString)

import Parser as P exposing ((|.), (|=), Parser)
import Set
import Time exposing (Posix, Zone)


type alias Time =
    ( Posix, Zone )


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



-- Path strings


toString : Time -> Path -> String
toString time path_ =
    pathString <|
        case path_ of
            Today ->
                datePath time

            Tomorrow ->
                datePath (shiftDays 1 time)

            Yesterday ->
                datePath (shiftDays -1 time)

            Project p ->
                p


datePath : Time -> String
datePath ( posix, zone ) =
    let
        year =
            Time.toYear zone posix |> String.fromInt

        day =
            Time.toDay zone posix |> String.fromInt

        month =
            case Time.toMonth zone posix of
                Time.Jan ->
                    "01"

                Time.Feb ->
                    "02"

                Time.Mar ->
                    "03"

                Time.Apr ->
                    "04"

                Time.May ->
                    "05"

                Time.Jun ->
                    "06"

                Time.Jul ->
                    "07"

                Time.Aug ->
                    "08"

                Time.Sep ->
                    "09"

                Time.Oct ->
                    "10"

                Time.Nov ->
                    "11"

                Time.Dec ->
                    "12"
    in
    String.join "-" [ year, month, day ]


shiftDays : Int -> Time -> Time
shiftDays shift =
    Tuple.mapFirst
        (Time.posixToMillis
            >> (+) (shift * 86400000)
            >> Time.millisToPosix
        )


pathString : String -> String
pathString string =
    ".shjo/" ++ string ++ ".shjo"
