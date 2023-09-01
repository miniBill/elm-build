module Ansi.Color.Extra exposing (gray)

import Ansi.Color as Color


rgb : Int -> Int -> Int -> Color.Color
rgb red green blue =
    Color.rgb
        { red = red
        , green = green
        , blue = blue
        }


gray : Color.Color
gray =
    rgb 0xC0 0xC0 0xC0
