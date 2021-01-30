port module FS exposing (Error(..), append, read, subscription, write)

import Json.Decode as Jd
import Json.Encode as Je exposing (Value)
import Utilities exposing (handleError)


port fsRequest : { method : String, args : List Value } -> Cmd msg


port fsResponse : (Value -> msg) -> Sub msg


type Error
    = NotFound String
    | OtherError Value
    | DecodingError Jd.Error


utf8 : Value
utf8 =
    Je.object [ ( "encoding", Je.string "utf8" ) ]


request : String -> List Value -> Cmd msg
request method args =
    fsRequest
        { method = method
        , args = args ++ [ utf8 ]
        }


read : String -> Cmd msg
read path =
    request "readFile" [ Je.string path ]


append : String -> String -> Cmd msg
append path string =
    request "appendFile" [ Je.string path, Je.string string ]


write : String -> String -> Cmd msg
write path string =
    request "writeFile" [ Je.string path, Je.string string ]


subscription : (Result Error Value -> msg) -> Sub msg
subscription map =
    Sub.map map (fsResponse fsResult)


fsResult : Value -> Result Error Value
fsResult =
    Jd.decodeValue fsResultDecoder
        >> handleError (Err << DecodingError)


fsResultDecoder : Jd.Decoder (Result Error Value)
fsResultDecoder =
    Jd.oneOf
        [ Jd.map Err <| Jd.field "error" fsErrorDecoder
        , Jd.map Ok Jd.value
        ]


fsErrorDecoder : Jd.Decoder Error
fsErrorDecoder =
    Jd.oneOf
        [ Jd.field "code" Jd.string
            |> Jd.andThen notFoundDecoder
        , Jd.map OtherError Jd.value
        ]


notFoundDecoder : String -> Jd.Decoder Error
notFoundDecoder code =
    case code of
        "ENOENT" ->
            Jd.field "path" Jd.string
                |> Jd.map NotFound

        _ ->
            Jd.fail ("Can't handle error " ++ code)
