module Flags exposing (Flags, decode)

import Command exposing (Command)
import Json.Decode as Jd exposing (Value)
import Time exposing (Posix, Zone)
import Utilities exposing (Time, handleError)


type alias Flags =
    Value


decode : Flags -> Result Jd.Error ( Time, Command )
decode =
    Jd.decodeValue flagsDecoder


flagsDecoder : Jd.Decoder ( Time, Command )
flagsDecoder =
    Jd.map2 Tuple.pair timezoneDecoder commandDecoder


timezoneDecoder : Jd.Decoder Time
timezoneDecoder =
    Jd.map2 Tuple.pair timeDecoder zoneDecoder


timeDecoder : Jd.Decoder Posix
timeDecoder =
    Jd.field "time" Jd.int
        |> Jd.map Time.millisToPosix


zoneDecoder : Jd.Decoder Zone
zoneDecoder =
    Jd.map2 (+) zoneOffsetDecoder midnightOffsetDecoder
        |> Jd.map Time.customZone
        |> Jd.map ((|>) [])


midnightOffsetDecoder : Jd.Decoder Int
midnightOffsetDecoder =
    Jd.oneOf
        [ Jd.field "SHJO_MIDNIGHT_OFFSET" Jd.string
            |> Jd.field "env"
            |> Jd.map (String.toInt >> Maybe.withDefault 0)
        , Jd.succeed 0
        ]


zoneOffsetDecoder : Jd.Decoder Int
zoneOffsetDecoder =
    Jd.field "zone" Jd.int


commandDecoder : Jd.Decoder Command
commandDecoder =
    argsDecoder
        |> Jd.andThen
            (Command.parse
                >> Result.map Jd.succeed
                >> handleError Jd.fail
            )


argsDecoder : Jd.Decoder String
argsDecoder =
    Jd.field "args" (Jd.list Jd.string)
        |> Jd.map (String.join " ")
