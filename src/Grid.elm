module Grid exposing (Cell, Grid, cellPosToString, cellToRecord, encode, getCells, initialize)

-- The Cell type will become what holds all the information of a cell as a user walks among them
-- For now though, I just want to get the app producing a grid
-- DamageTypes, Trap, and Event will all be moved out of here eventually

import Json.Decode as Decode
import Json.Encode as Encode


type Event
    = Event
        { name : String
        , description : String
        , effect : Int -- This will be changing to a standard Type itself
        }


type Cell
    = Cell
        { trap : Bool
        , pos : ( Int, Int )
        }


type Grid
    = Grid
        { width : Int
        , height : Int
        , data : List Cell
        }


cellToRecord : Cell -> { trap : Bool, pos : ( Int, Int ) }
cellToRecord (Cell { trap, pos }) =
    { trap = trap, pos = pos }


getCells : Grid -> List Cell
getCells (Grid { data }) =
    data


cellPosToString : Cell -> String
cellPosToString (Cell { pos }) =
    "X: " ++ String.fromInt (Tuple.first pos) ++ " Y: " ++ String.fromInt (Tuple.second pos)


initialize : Int -> Int -> Grid
initialize width height =
    let
        indicies =
            List.range 0 (height - 1)
                |> List.concatMap
                    (\y ->
                        List.range 0 (width - 1)
                            |> List.map (\x -> ( x, y ))
                    )
    in
    let
        data =
            indicies
                |> List.map (\( x, y ) -> Cell { trap = False, pos = ( x, y ) })
    in
    Grid { width = width, height = height, data = data }


encodeCell : List Cell -> Encode.Value
encodeCell c =
    Encode.list
        (\(Cell { trap, pos }) ->
            Encode.object
                [ ( "trap", Encode.bool trap )
                , ( "posX", Encode.int (Tuple.first pos) )
                , ( "posY", Encode.int (Tuple.second pos) )
                ]
        )
        c


encode : Int -> Grid -> String
encode indent (Grid { width, height, data }) =
    Encode.encode
        indent
        (Encode.object
            [ ( "width", Encode.int width )
            , ( "height", Encode.int height )
            , ( "data", encodeCell data )
            ]
        )
