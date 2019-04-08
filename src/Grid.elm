module Grid exposing (Grid, encode, initialize, isValid, toList)

-- The Cell type will become what holds all the information of a cell as a user walks among them
-- For now though, I just want to get the app producing a grid
-- DamageTypes, Trap, and Event will all be moved out of here eventually

import Cell exposing (Cell)
import Json.Decode as Decode
import Json.Encode as Encode


type Grid
    = Grid
        { width : Int
        , height : Int
        , data : List Cell
        }


isValid : ( Int, Int ) -> Grid -> Bool
isValid ( x, y ) (Grid { width, height }) =
    x >= 0 && x < width && y >= 0 && y < height


empty : { width : Int, height : Int } -> Grid
empty { width, height } =
    Grid
        { width = width
        , height = height
        , data = []
        }


toList : Grid -> List Cell
toList (Grid { data }) =
    data


{-| Generates a new Square Grid
-}
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
                |> List.map (\( x, y ) -> Cell.create { char = ".", pos = ( x, y ) })
    in
    Grid { width = width, height = height, data = data }


encode : Int -> Grid -> String
encode indent (Grid { width, height, data }) =
    Encode.encode
        indent
        (Encode.object
            [ ( "width", Encode.int width )
            , ( "height", Encode.int height )
            , ( "data", Cell.encode data )
            ]
        )
