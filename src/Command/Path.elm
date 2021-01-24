module Command.Path exposing (Path(..), parser, toString)

import Parser as P exposing ((|.), (|=), Parser)
import Set
import Time
import Time.Extra as TE
import Utilities exposing (Time, anyOf)


type Path
    = Date (Maybe Int) DatePath
    | RelativeDate RelativeDateKeyword
    | Tag String


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
        [ pathParser
        , P.succeed (RelativeDate (This KwDay))
        ]


pathParser : Parser Path
pathParser =
    P.succeed identity
        |. P.symbol "@"
        |= P.oneOf
            [ dateKeywordParser
            , datePathParser
            , tagPathParser
            , P.problem "The @ symbol must be followed by a date keyword, date or tag"
            ]
        |. closerParser


closerParser : Parser ()
closerParser =
    P.oneOf
        [ P.chompIf ((/=) ' ') |> P.andThen (always <| P.problem "path must end properly")
        , P.succeed ()
        ]


dateKeywordParser : Parser Path
dateKeywordParser =
    P.succeed RelativeDate
        |= P.oneOf
            [ P.succeed (This KwDay) |. P.keyword "today"
            , P.succeed (Next KwDay) |. P.keyword "tomorrow"
            , P.succeed (Last KwDay) |. P.keyword "yesterday"
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
    P.succeed identity
        |. P.chompWhile ((==) '0')
        |= P.int
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


tagPathParser : Parser Path
tagPathParser =
    P.succeed Tag
        |= P.variable
            { start = Char.isAlpha
            , inner = anyOf [ Char.isAlphaNum, (==) '-' ]
            , reserved = Set.empty
            }



-- Path strings


toString : Time -> Path -> String
toString time path =
    case path of
        Date year date ->
            datePath time year date

        Tag tag ->
            tag

        RelativeDate keyword ->
            keyword
                |> relativeToAbsoluteDate time
                |> toString time


toYearString : Time -> Maybe Int -> String
toYearString ( posix, zone ) =
    Maybe.withDefault (Time.toYear zone posix)
        >> String.fromInt


toMonthString : Time.Month -> String
toMonthString month =
    case month of
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


datePath : Time -> Maybe Int -> DatePath -> String
datePath time year date =
    case date of
        Year ->
            toYearString time year

        Quarter quarter ->
            String.concat
                [ toYearString time year
                , "-q"
                , String.fromInt quarter
                ]

        Month month ->
            String.concat
                [ toYearString time year
                , "-"
                , toMonthString month
                ]

        Week week ->
            String.concat
                [ toYearString time year
                , "-w"
                , String.fromInt week
                    |> String.padLeft 2 '0'
                ]

        Day month day ->
            String.concat
                [ toYearString time year
                , "-"
                , toMonthString month
                , "-"
                , String.fromInt day
                    |> String.padLeft 2 '0'
                ]


relativeToAbsoluteDate : Time -> RelativeDateKeyword -> Path
relativeToAbsoluteDate (( posix, zone ) as time) dateReference =
    let
        parts =
            TE.posixToParts zone posix
    in
    case dateReference of
        Next KwYear ->
            Date (Just (parts.year + 1)) Year

        Last KwYear ->
            Date (Just (parts.year - 1)) Year

        This keyword ->
            case keyword of
                KwMonth ->
                    Month parts.month
                        |> Date (Just parts.year)

                KwDay ->
                    Day parts.month parts.day
                        |> Date (Just parts.year)

                KwQuarter ->
                    posix
                        |> TE.floor TE.Year zone
                        |> TE.diff TE.Quarter zone posix
                        |> negate
                        |> (+) 1
                        |> Quarter
                        |> Date (Just parts.year)

                KwWeek ->
                    posix
                        |> TE.floor TE.Year zone
                        |> TE.diff TE.Week zone posix
                        |> negate
                        |> (+) 1
                        |> Week
                        |> Date (Just parts.year)

                KwYear ->
                    Date (Just parts.year) Year

                KwWeekday weekday ->
                    posix
                        |> TE.floor TE.Week zone
                        |> TE.ceiling (weekdayInterval weekday) zone
                        |> TE.posixToParts zone
                        |> (\p ->
                                Day p.month p.day
                                    |> Date (Just p.year)
                           )

        Next keyword ->
            relativeToAbsoluteDate ( shift 1 time keyword, zone ) (This keyword)

        Last keyword ->
            relativeToAbsoluteDate ( shift -1 time keyword, zone ) (This keyword)


weekdayInterval : Time.Weekday -> TE.Interval
weekdayInterval weekday =
    case weekday of
        Time.Mon ->
            TE.Monday

        Time.Tue ->
            TE.Tuesday

        Time.Wed ->
            TE.Wednesday

        Time.Thu ->
            TE.Thursday

        Time.Fri ->
            TE.Friday

        Time.Sat ->
            TE.Saturday

        Time.Sun ->
            TE.Sunday


shift : Int -> Time -> DateBlockKeyword -> Time.Posix
shift offset ( posix, zone ) keyword =
    let
        add interval =
            TE.add interval offset zone posix
    in
    case keyword of
        KwDay ->
            add TE.Day

        KwWeek ->
            add TE.Week

        KwMonth ->
            add TE.Month

        KwQuarter ->
            add TE.Quarter

        KwYear ->
            add TE.Year

        KwWeekday weekday ->
            add <| weekdayInterval weekday
