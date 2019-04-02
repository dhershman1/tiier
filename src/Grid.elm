module Grid exposing (Grid, produceGrid)

-- The Cell type will become what holds all the information of a cell as a user walks among them
-- For now though, I just want to get the app producing a grid


type Cell
    = Cell ( Int, Int )


type alias Grid =
    List Cell



-- fromList : a -> List ( Int, Int ) -> Grid
-- fromList value =
--     List.map (Cell value)


empty : Grid
empty =
    []


produceGrid : Int -> Grid
produceGrid cellCount =
    let
        x =
            0

        y =
            0
    in
    List.repeat cellCount (Cell ( x + 1, y + 1 ))
