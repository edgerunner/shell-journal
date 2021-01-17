module Page exposing (Line, Page, add, check, lineToString, parse, terminalOutput, toString)

import Entry exposing (Entry(..))
import Parser exposing ((|.), (|=), Parser)


type alias Page =
    List Line


type alias Line =
    ( Entry, String )


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
    Parser.succeed Tuple.pair
        |= bullet
        |= body
        |. Parser.symbol "\n"


body : Parser String
body =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompUntil "\n"


bullet : Parser Entry
bullet =
    Parser.oneOf (List.map bulletFor [ Task True, Task False, Event, Note ])
        |. Parser.spaces


bulletFor : Entry -> Parser Entry
bulletFor entry =
    Parser.succeed entry
        |. Parser.symbol (Entry.symbol entry)


check : Int -> Page -> Page
check lineNumber =
    List.indexedMap (checkMatchingIndex lineNumber)


checkMatchingIndex : Int -> Int -> Line -> Line
checkMatchingIndex lineNumberToCheck currentIndex ( entryType, lineBody ) =
    if ((lineNumberToCheck - 1) == currentIndex) && (entryType == Task False) then
        ( Task True, lineBody )

    else
        ( entryType, lineBody )


toString : Page -> String
toString =
    List.map lineToString
        >> String.join ""


lineToString : Line -> String
lineToString ( entry, lineBody ) =
    Entry.symbol entry
        ++ " "
        ++ lineBody
        ++ "\n"



-- Color output


terminalOutput : Int -> Page -> String
terminalOutput highlight lines =
    lines
        |> List.indexedMap (colorLine highlight)
        |> String.join "\n"


colorLine : Int -> Int -> Line -> String
colorLine highlight index ( entry, string ) =
    styleEscape
        (if highlight == index + 1 then
            [ style.brightYellow ]

         else
            [ style.black ]
        )
        ++ (String.padLeft 3 ' ' <| String.fromInt (index + 1))
        ++ styleEscape [ style.default ]
        ++ " "
        ++ colorCode entry
        ++ Entry.symbol entry
        ++ " "
        ++ string
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
    List.append p [ ( entry, content ) ]
