module Style exposing (Style, background, black, blue, bold, bright, cyan, default, dim, escape, green, hide, invert, italic, magenta, not, red, reset, underline, white, yellow)


type Style font color
    = Style String


type Color
    = Color Color


type Font
    = Font Font


escape : List (Style f c) -> String
escape styles =
    "\u{001B}[" ++ String.join ";" (List.map code styles) ++ "m"


code : Style f c -> String
code (Style code_) =
    code_


reset : Style f c
reset =
    Style "0"


bold : Style Font color
bold =
    Style "1"


dim : Style Font color
dim =
    Style "2"


italic : Style Font color
italic =
    Style "3"


underline : Style Font color
underline =
    Style "4"


invert : Style Font color
invert =
    Style "7"


hide : Style Font color
hide =
    Style "8"


not : Style Font () -> Style font color
not =
    let
        -- see https://en.wikipedia.org/wiki/ANSI_escape_code#Select_Graphic_Rendition_(SGR)_parameters
        fixNotBoldQuirk c =
            case c of
                "21" ->
                    "22"

                _ ->
                    c
    in
    map <| String.cons '2' >> fixNotBoldQuirk


black : Style font Color
black =
    Style "30"


red : Style font Color
red =
    Style "31"


green : Style font Color
green =
    Style "32"


yellow : Style font Color
yellow =
    Style "33"


blue : Style font Color
blue =
    Style "34"


magenta : Style font Color
magenta =
    Style "35"


cyan : Style font Color
cyan =
    Style "36"


white : Style font Color
white =
    Style "37"


default : Style font Color
default =
    Style "39"


bright : Style () Color -> Style () Color
bright style =
    case String.uncons (code style) of
        Just ( '3', color ) ->
            String.cons '9' color
                |> Style

        Just ( '4', color ) ->
            String.append "10" color
                |> Style

        _ ->
            style


background : Style () Color -> Style font Color
background style =
    case String.uncons (code style) of
        Just ( '3', color ) ->
            String.cons '4' color
                |> Style

        Just ( '9', color ) ->
            String.append "10" color
                |> Style

        _ ->
            map identity style


map : (String -> String) -> Style font1 color1 -> Style font2 color2
map fn =
    code >> fn >> Style
