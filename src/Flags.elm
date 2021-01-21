module Flags exposing (Flags, decode)

import Command exposing (Command)
import Json.Decode as Jd exposing (Value)
import Time exposing (Posix, Zone)
import Utilities exposing (handleError)


type alias Time =
    ( Posix, Zone )


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
    Jd.field "zone" Jd.int
        |> Jd.map Time.customZone
        |> Jd.map ((|>) [])


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
