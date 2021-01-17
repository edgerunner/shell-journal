module Page exposing (Line, Page, add, check, lineToString, parse, terminalOutput, toString)

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
        [ Parser.succeed (\lineTuple -> Parser.Loop (lineTuple :: reverseLines))
            |= line
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse reverseLines))
        ]


line : Parser Line
line =
    Parser.succeed Line
        |= bullet
        |= body
        |= star
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


star : Parser Bool
star =
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
check lineNumber =
    List.indexedMap (checkMatchingIndex lineNumber)


checkMatchingIndex : Int -> Int -> Line -> Line
checkMatchingIndex lineNumberToCheck currentIndex thisLine =
    if ((lineNumberToCheck - 1) == currentIndex) && (thisLine.bullet == Task False) then
        { thisLine
            | bullet = Task True
            , highlight = True
        }

    else
        thisLine


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
        ++ styleEscape [ style.default ]
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
        ++ styleEscape [ style.default ]


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
        ++ styleEscape [ style.default ]


style : { default : String, bold : String, dim : String, italic : String, white : String, blue : String, black : String, brightYellow : String }
style =
    { default = "0"
    , bold = "1"
    , dim = "2"
    , italic = "3"
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
