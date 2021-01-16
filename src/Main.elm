port module Main exposing (main)

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
        |> decodeCommand
        |> mapCommand


arguments : Flags -> List String
arguments =
    Result.withDefault []
        << decodeValue (list string)


decodeCommand : List String -> Maybe Command
decodeCommand args =
    case args of
        [] ->
            Just View

        [ "view" ] ->
            Just View

        "add" :: rest ->
            decodeAdd rest

        "check" :: rest ->
            decodeCheck rest

        _ ->
            Nothing


path : String
path =
    ".shjo/today.shjo"


mapCommand : Maybe Command -> ( Model, Cmd Msg )
mapCommand command =
    case command of
        Nothing ->
            ( Nothing, Cmd.none )

        Just View ->
            ( Just <| ViewFile, FS.read path )

        Just (Add entry contents) ->
            appendString entry contents
                |> FS.append path
                |> Tuple.pair (Just ViewFile)

        Just (Check lineNumber) ->
            ( Just <| CheckTask lineNumber, FS.read path )


decodeAdd : List String -> Maybe Command
decodeAdd args =
    let
        entry =
            case List.head args of
                Just "task" ->
                    Just (Task False)

                Just "event" ->
                    Just Event

                Just "note" ->
                    Just Note

                _ ->
                    Nothing

        contents =
            List.tail args
                |> Maybe.andThen nonEmpty
                |> Maybe.map (String.join " ")
    in
    Maybe.map2 Add entry contents


decodeCheck : List String -> Maybe Command
decodeCheck args =
    args
        |> List.head
        |> Maybe.andThen String.toInt
        |> Maybe.map Check


nonEmpty : List a -> Maybe (List a)
nonEmpty list =
    case list of
        [] ->
            Nothing

        _ ->
            Just list


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


type Command
    = View
    | Add Entry String
    | Check Int


type Entry
    = Task Bool
    | Event
    | Note


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
