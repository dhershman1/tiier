module Color exposing (Color, decode, encode, generateRgb, rgb, toRgb, toString)

import Json.Decode as Decode
import Json.Encode as Encode
import Random


type Color
    = Color { red : Int, green : Int, blue : Int }


rgb : Int -> Int -> Int -> Color
rgb red green blue =
    Color { red = red, green = green, blue = blue }


toRgb : Color -> { red : Int, green : Int, blue : Int }
toRgb (Color rawRgb) =
    rawRgb


toString : Color -> String
toString (Color { red, green, blue }) =
    "rgb("
        ++ String.fromInt red
        ++ ","
        ++ String.fromInt green
        ++ ","
        ++ String.fromInt blue
        ++ ")"


randoMinMax : ( Int, Int ) -> Random.Generator Int
randoMinMax t =
    Random.int (Tuple.first t) (Tuple.second t)


pluckTuple : List ( Int, Int ) -> ( Int, Int )
pluckTuple t =
    Maybe.withDefault ( 0, 0 ) (List.head t)


generateRgb : List ( Int, Int ) -> Random.Generator Color
generateRgb colors =
    let
        red =
            pluckTuple colors

        green =
            List.drop 1 colors

        blue =
            List.drop 2 colors
    in
    Random.map3 rgb (randoMinMax red) (randoMinMax (pluckTuple green)) (randoMinMax (pluckTuple blue))


decode : Decode.Decoder Color
decode =
    Decode.map3 rgb
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)
        (Decode.index 2 Decode.int)


encode : Color -> Encode.Value
encode (Color { red, green, blue }) =
    Encode.list Encode.int [ red, green, blue ]
