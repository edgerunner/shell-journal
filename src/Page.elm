module Page exposing (Line, LineError(..), Page, add, blank, check, clip, get, lineErrorMessage, lineToString, move, parse, star, strike, terminalOutput, toString)

import Bullet exposing (Bullet(..), TaskState(..))
import Command.Path as Path
import Flags exposing (Flags)
import Parser exposing ((|.), (|=), Parser)
import Style exposing (escape)
import Utilities exposing (ensure, optionalString)


type alias Page =
    List Line


type alias Line =
    { bullet : Bullet
    , body : String
    , star : Bool
    , strike : Bool
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
        |= strikeParser
        |. Parser.symbol "\n"
        |= Parser.succeed False
        |= Parser.succeed lineNumber


body : Parser String
body =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> (c /= '\n') && (c /= '★') && (c /= '⌧'))
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


strikeParser : Parser Bool
strikeParser =
    Parser.oneOf
        [ Parser.succeed True
            |. Parser.symbol strikeSymbol
        , Parser.succeed False
        ]


strikeSymbol : String
strikeSymbol =
    "⌧"


check : Int -> Page -> Result LineError Page
check =
    ensure
        (not << .strike)
        (InvalidOperation "That line is struck out. No point in checking it now.")
        >> Result.andThen
            (ensure
                (.bullet >> (==) (Task Pending))
                (InvalidOperation "It doesn't make sense to check a line that isn't a pending task")
            )
        >> Result.map (\l -> { l | bullet = Task Done })
        |> modifyByLineNumber


star : Int -> Page -> Result LineError Page
star =
    ensure
        (not << .strike)
        (InvalidOperation "That line is struck out. No point in starring it now.")
        >> Result.andThen
            (ensure
                (not << .star)
                (InvalidOperation "That line is already starred")
            )
        >> Result.map (\l -> { l | star = True })
        |> modifyByLineNumber


strike : Int -> Page -> Result LineError Page
strike =
    ensure
        (not << .strike)
        (InvalidOperation "That line is already struck out")
        >> Result.map (\l -> { l | strike = True })
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


toString : Flags -> Page -> String
toString flags =
    List.map (lineToString flags)
        >> String.join ""


lineToString : Flags -> Line -> String
lineToString flags thisLine =
    Bullet.symbol thisLine.bullet
        ++ " "
        ++ moveDestination flags thisLine
        ++ thisLine.body
        ++ optionalString (" " ++ starSymbol) thisLine.star
        ++ optionalString strikeSymbol thisLine.strike
        ++ "\n"



-- Color output


terminalOutput : Flags -> Page -> String
terminalOutput flags lines =
    lines
        |> List.map (colorLine flags)
        |> String.join "\n"


colorLine : Flags -> Line -> String
colorLine flags thisLine =
    escape
        (if thisLine.highlight then
            [ Style.bright Style.yellow ]

         else
            [ Style.black ]
        )
        ++ (String.padLeft 3 ' ' <| String.fromInt thisLine.lineNumber)
        ++ escape [ Style.reset ]
        ++ escapeIf thisLine.star [ Style.bold ]
        ++ " "
        ++ escapeIf thisLine.strike [ Style.dim, Style.background Style.black ]
        ++ colorCode thisLine.bullet
        ++ Bullet.symbol thisLine.bullet
        ++ " "
        ++ moveDestination flags thisLine
        ++ thisLine.body
        ++ optionalString yellowStar thisLine.star
        ++ escape [ Style.reset ]


escapeIf : Bool -> List (Style.Style f c) -> String
escapeIf cond styles =
    if cond then
        escape styles

    else
        ""


moveDestination : Flags -> Line -> String
moveDestination flags thisLine =
    case thisLine.bullet of
        Task (Moved destination lineNumber) ->
            "[" ++ Path.toString flags destination ++ ":" ++ String.fromInt lineNumber ++ "] "

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
          , strike = False
          , highlight = True
          , lineNumber = List.length p + 1
          }
        ]


move : String -> Int -> Int -> Page -> Result LineError Page
move destinationString destinationLineNumber =
    modifyByLineNumber
        (\line_ ->
            case line_.bullet of
                Task Pending ->
                    case Parser.run Path.pathParser destinationString of
                        Ok destination ->
                            Ok { line_ | bullet = Task (Moved destination destinationLineNumber) }

                        Err _ ->
                            Err <| InvalidOperation "Could not parse the destination path"

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
