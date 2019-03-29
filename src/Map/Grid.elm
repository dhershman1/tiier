module Map.Grid exposing (Grid, fromList)


type alias Cell a =
    { val : a
    , pos : ( Int, Int )
    }


type alias Grid a =
    List (Cell a)


fromList : a -> List ( Int, Int ) -> Grid a
fromList value =
    List.map (Cell value)
