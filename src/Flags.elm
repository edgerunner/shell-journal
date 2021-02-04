module Flags exposing (Flags, decode)

import Json.Decode as Jd exposing (Decoder, Value)
import Time exposing (Posix, Zone)
import Utilities exposing (handleError, shoehorn)


type alias Flags =
    { posix : Posix
    , zone : Zone
    , basePath : String
    }


decode : (String -> Result String command) -> Value -> Result Jd.Error ( command, Flags )
decode commandParser =
    Jd.decodeValue (decoder commandParser)


decoder : (String -> Result String command) -> Decoder ( command, Flags )
decoder commandParser =
    Jd.map2 Tuple.pair (commandDecoder commandParser) flagsDecoder


flagsDecoder : Decoder Flags
flagsDecoder =
    Jd.map3 Flags
        timeDecoder
        zoneDecoder
        basePathDecoder


timeDecoder : Decoder Posix
timeDecoder =
    Jd.field "time" Jd.int
        |> Jd.map Time.millisToPosix


zoneDecoder : Decoder Zone
zoneDecoder =
    Jd.map2 (+) zoneOffsetDecoder midnightOffsetDecoder
        |> Jd.map Time.customZone
        |> Jd.map ((|>) [])


midnightOffsetDecoder : Decoder Int
midnightOffsetDecoder =
    Jd.oneOf
        [ Jd.field "SHJO_MIDNIGHT_OFFSET" Jd.string
            |> Jd.field "env"
            |> Jd.map (String.toInt >> Maybe.withDefault 0)
        , Jd.succeed 0
        ]


zoneOffsetDecoder : Decoder Int
zoneOffsetDecoder =
    Jd.field "zone" Jd.int


commandDecoder : (String -> Result String command) -> Jd.Decoder command
commandDecoder commandParser =
    argsDecoder
        |> Jd.andThen
            (commandParser
                >> Result.map Jd.succeed
                >> handleError Jd.fail
            )


argsDecoder : Jd.Decoder String
argsDecoder =
    Jd.field "args" (Jd.list Jd.string)
        |> Jd.map (String.join " ")


basePathDecoder : Decoder String
basePathDecoder =
    Jd.map2 basePathResolver
        (envDecoder "SHJO_DIR" Jd.string)
        (envDecoder "HOME" Jd.string)


envDecoder : String -> Decoder a -> Decoder (Maybe a)
envDecoder var =
    Jd.field "env" << Jd.maybe << Jd.field var


basePathResolver : Maybe String -> Maybe String -> String
basePathResolver shjoDir homeDir =
    shjoDir
        |> Maybe.withDefault
            (homeDir
                |> Maybe.withDefault "."
                |> shoehorn (++) "/.shjo/"
            )
