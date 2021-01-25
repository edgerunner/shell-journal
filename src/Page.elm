module Page exposing (Line, Page, add, blank, check, clip, lineToString, parse, star, terminalOutput, toString)

import Bullet exposing (Bullet(..), TaskState(..))
import Parser exposing ((|.), (|=), Parser)
import Utilities exposing (only, optionalString)


type alias Page =
    List Line


type alias Line =
    { bullet : Bullet
    , body : String
    , star : Bool
    , highlight : Bool
    , lineNumber : Int
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
            |= line (List.length reverseLines + 1)
        , Parser.succeed <| Parser.Done (List.reverse reverseLines)
        ]


line : Int -> Parser Line
line lineNumber =
    Parser.succeed Line
        |= Bullet.parser
        |= body
        |= starParser
        |. Parser.symbol "\n"
        |= Parser.succeed False
        |= Parser.succeed lineNumber


body : Parser String
body =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> (c /= '\n') && (c /= '★'))
        |> Parser.getChompedString
        |> Parser.map String.trim


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


check : Int -> Page -> Page
check =
    only (.bullet >> (==) (Task Pending))
        >> Maybe.map (\l -> { l | bullet = Task Done })
        |> modifyByLineNumber


star : Int -> Page -> Page
star =
    only (not << .star)
        >> Maybe.map (\l -> { l | star = True })
        |> modifyByLineNumber


modifyByLineNumber : (Line -> Maybe Line) -> Int -> Page -> Page
modifyByLineNumber modify lineNumber =
    List.map
        (\thisLine ->
            if lineNumber == thisLine.lineNumber then
                modify { thisLine | highlight = True }
                    |> Maybe.withDefault thisLine

            else
                thisLine
        )


blank : Page
blank =
    []


toString : Page -> String
toString =
    List.map lineToString
        >> String.join ""


lineToString : Line -> String
lineToString thisLine =
    Bullet.symbol thisLine.bullet
        ++ " "
        ++ thisLine.body
        ++ optionalString (" " ++ starSymbol) thisLine.star
        ++ "\n"



-- Color output


terminalOutput : Page -> String
terminalOutput lines =
    lines
        |> List.map colorLine
        |> String.join "\n"


colorLine : Line -> String
colorLine thisLine =
    styleEscape
        (if thisLine.highlight then
            [ style.brightYellow ]

         else
            [ style.black ]
        )
        ++ (String.padLeft 3 ' ' <| String.fromInt thisLine.lineNumber)
        ++ styleEscape [ style.reset ]
        ++ (if thisLine.star then
                styleEscape [ style.bold ]

            else
                ""
           )
        ++ " "
        ++ colorCode thisLine.bullet
        ++ Bullet.symbol thisLine.bullet
        ++ " "
        ++ thisLine.body
        ++ optionalString yellowStar thisLine.star
        ++ styleEscape [ style.reset ]


colorCode : Bullet -> String
colorCode bullet =
    case bullet of
        Task Pending ->
            styleEscape [ style.default ]

        Task Done ->
            styleEscape [ style.dim ]

        Task (Moved _) ->
            styleEscape [ style.green ]

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


style : { reset : String, bold : String, dim : String, italic : String, default : String, white : String, green : String, blue : String, black : String, brightYellow : String }
style =
    { reset = "0"
    , bold = "1"
    , dim = "2"
    , italic = "3"
    , default = "39"
    , white = "37"
    , blue = "34"
    , green = "32"
    , black = "30"
    , brightYellow = "93"
    }


styleEscape : List String -> String
styleEscape inner =
    "\u{001B}[" ++ String.join ";" inner ++ "m"



-- Add line


add : Bullet -> String -> Page -> Page
add bullet content p =
    List.append p
        [ { bullet = bullet
          , body = content
          , star = False
          , highlight = True
          , lineNumber = List.length p + 1
          }
        ]


clip : Int -> Page -> Page
clip =
    Utilities.clip .highlight
