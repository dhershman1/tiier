module Cell exposing (Cell, create, encode, posToString, toRecord)

import Json.Decode as Decode
import Json.Encode as Encode


type Cell
    = Cell
        { char : String
        , pos : ( Int, Int )
        }


create : { char : String, pos : ( Int, Int ) } -> Cell
create rec =
    Cell rec


toRecord : Cell -> { char : String, pos : ( Int, Int ) }
toRecord (Cell { char, pos }) =
    { char = char, pos = pos }


posToString : Cell -> String
posToString (Cell { pos }) =
    "X: " ++ String.fromInt (Tuple.first pos) ++ " Y: " ++ String.fromInt (Tuple.second pos)


encode : List Cell -> Encode.Value
encode c =
    Encode.list
        (\(Cell { char, pos }) ->
            Encode.object
                [ ( "char", Encode.string char )
                , ( "posX", Encode.int (Tuple.first pos) )
                , ( "posY", Encode.int (Tuple.second pos) )
                ]
        )
        c
