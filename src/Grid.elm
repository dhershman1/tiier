module Grid exposing (Cell, Grid, empty, fromList, mapToList)

-- The Cell type will become what holds all the information of a cell as a user walks among them
-- For now though, I just want to get the app producing a grid


type alias Cell a =
    { details : a
    , pos : ( Int, Int )
    }


type alias Grid a =
    List (Cell a)



-- type GridDetails
--     = GridDetails
--         { passable : Bool
--         , trap : Bool
--         }


fromList : a -> List ( Int, Int ) -> Grid a
fromList value =
    List.map (Cell value)


mapToList : (a -> ( Int, Int ) -> b) -> Grid a -> List b
mapToList fun =
    List.map (\{ details, pos } -> fun details pos)


empty : Grid a
empty =
    []
