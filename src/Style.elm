module Style exposing (Style, background, black, blue, bold, bright, cyan, default, dim, escape, green, italic, red, regular, reset, white, yellow)


type Style
    = Style String


escape : List Style -> String
escape styles =
    "\u{001B}[" ++ String.join ";" (List.map code styles) ++ "m"


code : Style -> String
code (Style code_) =
    code_


reset : Style
reset =
    Style "0"


bold : Style
bold =
    Style "1"


dim : Style
dim =
    Style "2"


italic : Style
italic =
    Style "3"


default : Style
default =
    Style "39"


white : Style
white =
    Style "37"


blue : Style
blue =
    Style "34"


green : Style
green =
    Style "32"


black : Style
black =
    Style "30"


yellow : Style
yellow =
    Style "33"


regular : Style
regular =
    Style "22"


red : Style
red =
    Style "31"


cyan : Style
cyan =
    Style "36"


bright : Style -> Style
bright style =
    case String.uncons (code style) of
        Just ( '3', color ) ->
            String.cons '9' color
                |> Style

        _ ->
            style


background : Style -> Style
background style =
    case String.uncons (code style) of
        Just ( '3', color ) ->
            String.cons '4' color
                |> Style

        _ ->
            style
