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
    colorEscape
        (if highlight == index + 1 then
            "93"

         else
            "30"
        )
        ++ (String.padLeft 3 ' ' <| String.fromInt (index + 1))
        ++ colorEscape "0"
        ++ " "
        ++ colorCode entry
        ++ Entry.symbol entry
        ++ " "
        ++ string
        ++ colorEscape "0"


colorCode : Entry -> String
colorCode entry =
    case entry of
        Task False ->
            colorEscape "0"

        Task True ->
            colorEscape "2"

        Event ->
            colorEscape "37"

        Note ->
            colorEscape "3;34"


colorEscape : String -> String
colorEscape inner =
    "\u{001B}[" ++ inner ++ "m"



-- Add line


add : Entry -> String -> Page -> Page
add entry content p =
    List.append p [ ( entry, content ) ]
