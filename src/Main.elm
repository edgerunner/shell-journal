port module Main exposing (main)

import Command exposing (Command(..), Entry(..))
import FS
import Json.Decode exposing (decodeValue, list, string)
import Json.Encode exposing (Value)
import Parser exposing ((|.), (|=), Parser)


port put : String -> Cmd msg


type alias Model =
    Maybe (Value -> Msg)


type alias Flags =
    Value


type Msg
    = ViewFile Value
    | CheckTask Int Value


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    arguments flags
        |> Command.decode
        |> mapCommand


arguments : Flags -> List String
arguments =
    Result.withDefault []
        << decodeValue (list string)


path : String
path =
    ".shjo/today.shjo"


mapCommand : Maybe Command -> ( Model, Cmd Msg )
mapCommand maybeCommand =
    maybeCommand
        |> Maybe.map
            (\command ->
                case command of
                    View ->
                        ( Just ViewFile, FS.read path )

                    Add entry contents ->
                        appendString entry contents
                            |> FS.append path
                            |> Tuple.pair Nothing

                    Check lineNumber ->
                        ( Just <| CheckTask lineNumber, FS.read path )
            )
        |> Maybe.withDefault ( Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        ViewFile response ->
            response
                |> decodeValue string
                |> Result.withDefault "Parse error"
                |> parse
                |> put
                |> Tuple.pair Nothing

        CheckTask checkIndex response ->
            response
                |> decodeValue string
                |> Result.withDefault "Parse error"
                |> checkLine checkIndex
                |> FS.write path
                |> Tuple.pair Nothing


checkLine : Int -> String -> String
checkLine index inputBody =
    inputBody
        |> Parser.run file
        |> Result.withDefault []
        |> List.indexedMap (checkMatchingIndex index)
        |> List.map lineToString
        |> String.join ""


checkMatchingIndex : Int -> Int -> Line -> Line
checkMatchingIndex targetIndex currentIndex ( entryType, lineBody ) =
    if ((targetIndex - 1) == currentIndex) && (entryType == Task False) then
        ( Task True, lineBody )

    else
        ( entryType, lineBody )


lineToString : Line -> String
lineToString ( entryType, lineBody ) =
    symbolFor entryType
        ++ " "
        ++ lineBody
        ++ "\n"


parse : String -> String
parse data =
    case Parser.run file data of
        Err _ ->
            "Error: malformed file"

        Ok lines ->
            lines
                |> List.indexedMap colorLine
                |> String.join "\n"


colorLine : Int -> Line -> String
colorLine index ( entry, string ) =
    colorEscape "30"
        ++ (String.padLeft 3 ' ' <| String.fromInt (index + 1))
        ++ colorEscape "0"
        ++ " "
        ++ colorCode entry
        ++ symbolFor entry
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
            colorEscape "1;37"

        Note ->
            colorEscape "3;34"


colorEscape : String -> String
colorEscape inner =
    "\u{001B}[" ++ inner ++ "m"


subscriptions : Model -> Sub Msg
subscriptions model =
    model
        |> Maybe.map FS.subscription
        |> Maybe.withDefault Sub.none


appendString : Entry -> String -> String
appendString entry content =
    symbolFor entry ++ " " ++ content ++ "\n"


symbolFor : Entry -> String
symbolFor entry =
    case entry of
        Task False ->
            "·"

        Task True ->
            "╳"

        Event ->
            "○"

        Note ->
            " "



-- Parse file


type alias File =
    List Line


type alias Line =
    ( Entry, String )


file : Parser File
file =
    Parser.loop [] fileHelp


fileHelp : File -> Parser (Parser.Step File File)
fileHelp reverseLines =
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
        |. Parser.symbol (symbolFor entry)
