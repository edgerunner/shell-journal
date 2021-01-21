port module FS exposing (append, read, subscription, write)

import Json.Decode as Jd
import Json.Encode as Je exposing (Value)
import Utilities exposing (handleError)


port fsRequest : { method : String, args : List Value } -> Cmd msg


port fsResponse : (Value -> msg) -> Sub msg


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


subscription : (Result Value Value -> msg) -> Sub msg
subscription map =
    Sub.map map (fsResponse fsResult)


fsResult : Value -> Result Value Value
fsResult =
    Jd.decodeValue fsResultDecoder
        >> handleError (always <| Err Je.null)


fsResultDecoder : Jd.Decoder (Result Value Value)
fsResultDecoder =
    Jd.oneOf
        [ Jd.map Err <| Jd.field "error" Jd.value
        , Jd.map Ok Jd.value
        ]
