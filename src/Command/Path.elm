module Command.Path exposing (Path(..), parser, toString)

import Parser as P exposing ((|.), (|=), Parser)
import Set
import Time
import Utilities exposing (Time, anyOf)


type Path
    = Date (Maybe Int) DatePath
    | RelativeDate RelativeDateKeyword
    | Hashtag String


type DatePath
    = Day Time.Month Int
    | Week Int
    | Month Time.Month
    | Quarter Int
    | Year


type RelativeDateKeyword
    = This DateBlockKeyword
    | Next DateBlockKeyword
    | Last DateBlockKeyword
    | Today
    | Yesterday
    | Tomorrow


type DateBlockKeyword
    = KwDay
    | KwWeekday Time.Weekday
    | KwWeek
    | KwMonth
    | KwQuarter
    | KwYear


parser : Parser Path
parser =
    P.oneOf
        [ dateParser
        , hashtagPathParser
        , P.succeed (RelativeDate Today)
        ]
        |. chompIf ((==) ' ')


dateParser : Parser Path
dateParser =
    P.succeed identity
        |. P.symbol "@"
        |= P.oneOf
            [ dateKeywordParser
            , datePathParser
            , P.problem "The @ symbol must be followed by a date keyword or date"
            ]


dateKeywordParser : Parser Path
dateKeywordParser =
    P.succeed RelativeDate
        |= P.oneOf
            [ P.succeed Today |. P.keyword "today"
            , P.succeed Tomorrow |. P.keyword "tomorrow"
            , P.succeed Yesterday |. P.keyword "yesterday"
            , dateBlockKeywordParser "this" This
            , dateBlockKeywordParser "next" Next
            , dateBlockKeywordParser "last" Last
            , dateBlockKeywordParser "" This
            ]


dateBlockKeywordParser :
    String
    -> (DateBlockKeyword -> RelativeDateKeyword)
    -> Parser RelativeDateKeyword
dateBlockKeywordParser keyword relativeDate =
    P.succeed relativeDate
        |. P.keyword keyword
        |. P.spaces
        |= P.oneOf
            [ P.succeed KwDay |. P.keyword "day"
            , P.succeed KwWeek |. P.keyword "week"
            , P.succeed KwMonth |. P.keyword "month"
            , P.succeed KwQuarter |. P.keyword "quarter"
            , P.succeed KwYear |. P.keyword "year"
            , P.succeed KwWeekday |= weekdayParser
            ]


datePathParser : Parser Path
datePathParser =
    yearParser
        |> P.andThen dateBlockParser


dateBlockParser : (DatePath -> Path) -> Parser Path
dateBlockParser year =
    P.succeed year
        |= P.oneOf
            [ P.succeed Quarter
                |. P.symbol "q"
                |= constrainedIntParser 1 4
            , P.succeed Week
                |. P.symbol "w"
                |= constrainedIntParser 1 53
            , P.succeed Day
                |= monthParser
                |. maybeDash
                |= constrainedIntParser 1 31
            , P.succeed Month
                |= monthParser
            , P.succeed (year Year)
                |> P.andThen
                    (\date ->
                        case date of
                            Date (Just _) Year ->
                                P.succeed Year

                            _ ->
                                P.problem "There needs to be some sort of date"
                    )
            ]


monthParser : Parser Time.Month
monthParser =
    P.oneOf
        [ P.succeed Time.Jan |. keywords [ "01", "jan", "january" ]
        , P.succeed Time.Feb |. keywords [ "02", "feb", "february" ]
        , P.succeed Time.Mar |. keywords [ "03", "mar", "march" ]
        , P.succeed Time.Apr |. keywords [ "04", "apr", "april" ]
        , P.succeed Time.May |. keywords [ "05", "may", "may" ]
        , P.succeed Time.Jun |. keywords [ "06", "jun", "june" ]
        , P.succeed Time.Jul |. keywords [ "07", "jul", "july" ]
        , P.succeed Time.Aug |. keywords [ "08", "aug", "august" ]
        , P.succeed Time.Sep |. keywords [ "09", "sep", "september" ]
        , P.succeed Time.Oct |. keywords [ "10", "oct", "october" ]
        , P.succeed Time.Nov |. keywords [ "11", "nov", "november" ]
        , P.succeed Time.Dec |. keywords [ "12", "dec", "december" ]
        ]


yearParser : Parser (DatePath -> Path)
yearParser =
    P.oneOf
        [ P.succeed (Date << Just)
            |= P.backtrackable (constrainedIntParser 2000 3000)
            |. maybeDash
        , P.succeed (Date Nothing)
        ]


constrainedIntParser : Int -> Int -> Parser Int
constrainedIntParser min max =
    P.int
        |> P.andThen
            (\int ->
                if int >= min && int <= max then
                    P.succeed int

                else
                    P.problem "Integer out of acceptable range"
            )


maybeDash : Parser ()
maybeDash =
    P.oneOf [ P.symbol "-", P.succeed () ]


weekdayParser : Parser Time.Weekday
weekdayParser =
    P.oneOf
        [ P.succeed Time.Mon |. keywords [ "mon", "monday" ]
        , P.succeed Time.Tue |. keywords [ "tue", "tuesday" ]
        , P.succeed Time.Wed |. keywords [ "wed", "wednesday" ]
        , P.succeed Time.Thu |. keywords [ "thu", "thursday" ]
        , P.succeed Time.Fri |. keywords [ "fri", "friday" ]
        , P.succeed Time.Sat |. keywords [ "sat", "saturday" ]
        , P.succeed Time.Sun |. keywords [ "sun", "sunday" ]
        ]


keywords : List String -> Parser ()
keywords =
    List.map (P.keyword >> P.backtrackable) >> P.oneOf


hashtagPathParser : Parser Path
hashtagPathParser =
    P.succeed Hashtag
        |. P.symbol "#"
        |= P.oneOf
            [ P.variable
                { start = Char.isAlpha
                , inner = anyOf [ Char.isAlphaNum, (==) '-' ]
                , reserved = Set.empty
                }
            , P.problem "# must be followed by a tag name"
            ]



-- Path strings


toString : Time -> Path -> String
toString time path_ =
    pathString <|
        case path_ of
            RelativeDate Today ->
                datePath time

            Hashtag tag ->
                tag

            _ ->
                Debug.todo "date path strings"


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
