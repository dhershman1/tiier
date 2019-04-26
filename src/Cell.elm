module Cell exposing (Cell, create, decode, encode, posToString, toRecord)

import Json.Decode as Decode
import Json.Encode as Encode


type Cast
    = Wall
    | Floor
    | Tree
    | Rock


type alias CellRec =
    { char : String
    , passable : Bool
    , color : String
    , pos : ( Int, Int )
    }


type Cell
    = Cell CellRec


create : CellRec -> Cell
create rec =
    Cell rec


toRecord : Cell -> CellRec
toRecord (Cell { char, color, passable, pos }) =
    { char = char, color = color, passable = passable, pos = pos }


posToString : Cell -> String
posToString (Cell { pos }) =
    String.fromInt (Tuple.first pos) ++ ", " ++ String.fromInt (Tuple.second pos)


posFromString : String -> Maybe ( Int, Int )
posFromString s =
    case String.split ", " s of
        x :: y :: [] ->
            let
                newX =
                    case String.toInt x of
                        Nothing ->
                            0

                        Just iX ->
                            iX

                newY =
                    case String.toInt y of
                        Nothing ->
                            0

                        Just iY ->
                            iY
            in
            Just <| ( newX, newY )

        _ ->
            Nothing



-- encode : List Cell -> Encode.Value
-- encode c =
--     Encode.list
--         (\(Cell { char, passable, pos }) ->
--             Encode.object
--                 [ ( "char", Encode.string char )
--                 , ( "passable", Encode.bool passable )
--                 , ( "posX", Encode.int (Tuple.first pos) )
--                 , ( "posY", Encode.int (Tuple.second pos) )
--                 ]
--         )
--         c


encode : Cell -> Encode.Value
encode (Cell { char, pos }) =
    Encode.object
        [ ( "char", Encode.string char )
        , ( "posX", Encode.int (Tuple.first pos) )
        , ( "posY", Encode.int (Tuple.second pos) )
        ]


decode : Decode.Decoder Cell
decode =
    Decode.map5
        (\char color passable posX posY ->
            Cell
                { char = char
                , color = color
                , passable = passable
                , pos = ( posX, posY )
                }
        )
        (Decode.field "char" Decode.string)
        (Decode.field "color" Decode.string)
        (Decode.field "passable" Decode.bool)
        (Decode.field "posX" Decode.int)
        (Decode.field "posY" Decode.int)
