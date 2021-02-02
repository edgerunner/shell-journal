module Page exposing (Line, LineError(..), Page, add, blank, check, clip, get, lineErrorMessage, lineToString, move, parse, star, terminalOutput, toString)

import Bullet exposing (Bullet(..), TaskState(..))
import Parser exposing ((|.), (|=), Parser)
import Style exposing (escape)
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


check : Int -> Page -> Result LineError Page
check =
    only (.bullet >> (==) (Task Pending))
        >> Maybe.map (\l -> { l | bullet = Task Done })
        >> Result.fromMaybe (InvalidOperation "It doesn't make sense to check a line that isn't a pending task")
        |> modifyByLineNumber


star : Int -> Page -> Result LineError Page
star =
    only (not << .star)
        >> Maybe.map (\l -> { l | star = True })
        >> Result.fromMaybe (InvalidOperation "That line is already starred")
        |> modifyByLineNumber


type LineError
    = LineNotFound Int
    | InvalidOperation String


modifyByLineNumber : (Line -> Result LineError Line) -> Int -> Page -> Result LineError Page
modifyByLineNumber modify lineNumber page_ =
    case page_ of
        [] ->
            Err <| LineNotFound lineNumber

        line_ :: rest ->
            if lineNumber == line_.lineNumber then
                modify { line_ | highlight = True }
                    |> Result.andThen
                        ((::) >> (|>) rest >> Ok)

            else
                modifyByLineNumber modify lineNumber rest
                    |> Result.map ((::) line_)


lineErrorMessage : String -> LineError -> String
lineErrorMessage pageTitle error =
    case error of
        LineNotFound lineNumber ->
            String.concat
                [ "Line number "
                , escape [ Style.bold ]
                , String.fromInt lineNumber
                , escape [ Style.not Style.bold ]
                , " is not in the page for "
                , escape [ Style.bold ]
                , pageTitle
                , escape [ Style.reset ]
                ]

        InvalidOperation errorMessage ->
            String.concat
                [ "Invalid operation on "
                , pageTitle
                , ": "
                , errorMessage
                ]


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
        ++ moveDestination thisLine
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
    escape
        (if thisLine.highlight then
            [ Style.bright Style.yellow ]

         else
            [ Style.black ]
        )
        ++ (String.padLeft 3 ' ' <| String.fromInt thisLine.lineNumber)
        ++ escape [ Style.reset ]
        ++ (if thisLine.star then
                escape [ Style.bold ]

            else
                ""
           )
        ++ " "
        ++ colorCode thisLine.bullet
        ++ Bullet.symbol thisLine.bullet
        ++ " "
        ++ moveDestination thisLine
        ++ thisLine.body
        ++ optionalString yellowStar thisLine.star
        ++ escape [ Style.reset ]


moveDestination : Line -> String
moveDestination thisLine =
    case thisLine.bullet of
        Task (Moved destination lineNumber) ->
            "[" ++ destination ++ ":" ++ String.fromInt lineNumber ++ "] "

        _ ->
            ""


colorCode : Bullet -> String
colorCode bullet =
    case bullet of
        Task Pending ->
            escape [ Style.default ]

        Task Done ->
            escape [ Style.dim ]

        Task (Moved _ _) ->
            escape [ Style.green ]

        Event ->
            escape [ Style.white ]

        Note ->
            escape [ Style.italic, Style.blue ]


yellowStar : String
yellowStar =
    " "
        ++ escape [ Style.bright Style.yellow ]
        ++ starSymbol
        ++ escape [ Style.reset ]



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


move : String -> Int -> Int -> Page -> Result LineError Page
move destination destinationLineNumber =
    modifyByLineNumber
        (\line_ ->
            case line_.bullet of
                Task Pending ->
                    Ok { line_ | bullet = Task (Moved destination destinationLineNumber) }

                _ ->
                    Err <| InvalidOperation "I only move pending tasks. Sorry."
        )


get : Int -> Page -> Maybe Line
get lineNumber lines =
    case lines of
        [] ->
            Nothing

        first :: rest ->
            if first.lineNumber == lineNumber then
                Just first

            else
                get lineNumber rest


clip : Int -> Page -> Page
clip =
    Utilities.clip .highlight
