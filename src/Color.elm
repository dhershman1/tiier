module Color exposing (Color, decode, encode, randomRgb, rgb, toRgb, toString)

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


randomRgb : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> Random.Seed -> ( String, Random.Seed )
randomRgb ( rMin, rMax ) ( gMin, gMax ) ( bMin, bMax ) seed =
    let
        ( ranRgb, nextSeed ) =
            Random.step
                (Random.map3 rgb
                    (Random.int rMin rMax)
                    (Random.int gMin gMax)
                    (Random.int bMin bMax)
                )
                seed
    in
    ( toString ranRgb, nextSeed )


decode : Decode.Decoder Color
decode =
    Decode.map3 rgb
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)
        (Decode.index 2 Decode.int)


encode : Color -> Encode.Value
encode (Color { red, green, blue }) =
    Encode.list Encode.int [ red, green, blue ]
