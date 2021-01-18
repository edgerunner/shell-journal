module Page exposing (Line, Page, add, check, lineToString, parse, star, terminalOutput, toString)

import Entry exposing (Entry(..))
import Parser exposing ((|.), (|=), Parser)


type alias Page =
    List Line


type alias Line =
    { bullet : Entry
    , body : String
    , star : Bool
    , highlight : Bool
    }


parse : String -> Result (List Parser.DeadEnd) Page
parse =
    Parser.run page


page : Parser Page
page =
    Parser.loop [] pageHelp


pageHelp : Page -> Parser (Parser.Step Page Page)
pageHelp reverseLines =
    Parser.oneOf
        [ Parser.succeed ((::) >> (|>) reverseLines >> Parser.Loop)
            |= line
        , Parser.succeed ()
            |> Parser.map (always <| Parser.Done (List.reverse reverseLines))
        ]


line : Parser Line
line =
    Parser.succeed Line
        |= bullet
        |= body
        |= starParser
        |. Parser.symbol "\n"
        |= Parser.succeed False


body : Parser String
body =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> (c /= '\n') && (c /= '★'))
        |> Parser.getChompedString
        |> Parser.map String.trim


bullet : Parser Entry
bullet =
    Parser.oneOf (List.map bulletFor [ Task True, Task False, Event, Note ])
        |. Parser.spaces


starParser : Parser Bool
starParser =
    Parser.oneOf
        [ Parser.succeed True
            |. Parser.symbol starSymbol
        , Parser.succeed False
        ]


starSymbol : String
starSymbol =
    "★"


bulletFor : Entry -> Parser Entry
bulletFor entry =
    Parser.succeed entry
        |. Parser.symbol (Entry.symbol entry)


check : Int -> Page -> Page
check =
    only (.bullet >> (==) (Task False))
        >> Maybe.map (\l -> { l | bullet = Task True })
        |> modifyByLineNumber


star : Int -> Page -> Page
star =
    only (not << .star)
        >> Maybe.map (\l -> { l | star = True })
        |> modifyByLineNumber


modifyByLineNumber : (Line -> Maybe Line) -> Int -> Page -> Page
modifyByLineNumber modify lineNumber =
    List.indexedMap
        (\index thisLine ->
            if (lineNumber - 1) == index then
                modify { thisLine | highlight = True }
                    |> Maybe.withDefault thisLine

            else
                thisLine
        )


only : (a -> Bool) -> a -> Maybe a
only predicate value =
    if predicate value then
        Just value

    else
        Nothing


toString : Page -> String
toString =
    List.map lineToString
        >> String.join ""


lineToString : Line -> String
lineToString thisLine =
    Entry.symbol thisLine.bullet
        ++ " "
        ++ thisLine.body
        ++ optionalString (" " ++ starSymbol) thisLine.star
        ++ "\n"


optionalString : String -> Bool -> String
optionalString string condition =
    if condition then
        string

    else
        ""



-- Color output


terminalOutput : Page -> String
terminalOutput lines =
    lines
        |> List.indexedMap colorLine
        |> String.join "\n"


colorLine : Int -> Line -> String
colorLine index thisLine =
    styleEscape
        (if thisLine.highlight then
            [ style.brightYellow ]

         else
            [ style.black ]
        )
        ++ (String.padLeft 3 ' ' <| String.fromInt (index + 1))
        ++ styleEscape [ style.reset ]
        ++ (if thisLine.star then
                styleEscape [ style.bold ]

            else
                ""
           )
        ++ " "
        ++ colorCode thisLine.bullet
        ++ Entry.symbol thisLine.bullet
        ++ " "
        ++ thisLine.body
        ++ optionalString yellowStar thisLine.star
        ++ styleEscape [ style.reset ]


colorCode : Entry -> String
colorCode entry =
    case entry of
        Task False ->
            styleEscape [ style.default ]

        Task True ->
            styleEscape [ style.dim ]

        Event ->
            styleEscape [ style.white ]

        Note ->
            styleEscape [ style.italic, style.blue ]


yellowStar : String
yellowStar =
    " "
        ++ styleEscape [ style.brightYellow ]
        ++ starSymbol
        ++ styleEscape [ style.reset ]


style : { reset : String, bold : String, dim : String, italic : String, default : String, white : String, blue : String, black : String, brightYellow : String }
style =
    { reset = "0"
    , bold = "1"
    , dim = "2"
    , italic = "3"
    , default = "39"
    , white = "37"
    , blue = "34"
    , black = "30"
    , brightYellow = "93"
    }


styleEscape : List String -> String
styleEscape inner =
    "\u{001B}[" ++ String.join ";" inner ++ "m"



-- Add line


add : Entry -> String -> Page -> Page
add entry content p =
    List.append p
        [ { bullet = entry
          , body = content
          , star = False
          , highlight = True
          }
        ]
