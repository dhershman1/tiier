module Map.Pieces exposing (Cell, floor, hallwayEW, hallwayNS, pathEW, pathNS, strToTerrain, terrainToClass, wall, water, waterEW, waterNS)

{-| Contains a bunch of pieces for map generation
-}


type Terrain
    = Water
    | Wall
    | Floor
    | Forest
    | TownRoad
    | Abyss


type alias Cell =
    { char : String
    , passable : Bool
    , terrain : Terrain
    , pos : ( Int, Int )
    }


type alias Piece =
    { name : String
    , posList : List Cell
    }


strToTerrain : String -> Terrain
strToTerrain str =
    case str of
        "wall" ->
            Wall

        "water" ->
            Water

        "floor" ->
            Floor

        "forest" ->
            Forest

        _ ->
            Abyss


terrainToClass : Cell -> String
terrainToClass { terrain } =
    case terrain of
        Wall ->
            "wall"

        Water ->
            "water"

        Floor ->
            "floor"

        Forest ->
            "forest"

        TownRoad ->
            "town-road"

        Abyss ->
            "abyss"


wall : ( Int, Int ) -> Cell
wall pos =
    Cell "#" False Wall pos


floor : ( Int, Int ) -> Cell
floor pos =
    Cell "." True Floor pos


water : ( Int, Int ) -> Cell
water pos =
    Cell "~" True Water pos


pathNS : Piece
pathNS =
    { name = "Path NS"
    , posList = [ wall ( -1, 0 ), floor ( 0, 0 ), wall ( 1, 0 ) ]
    }


pathEW : Piece
pathEW =
    { name = "Path EW"
    , posList = [ wall ( 0, -1 ), floor ( 0, 0 ), wall ( 0, 1 ) ]
    }


waterNS : Piece
waterNS =
    { name = "Water NS"
    , posList = [ water ( 0, 0 ), water ( 1, 0 ) ]
    }


waterEW : Piece
waterEW =
    { name = "Water EW"
    , posList = [ water ( 0, 0 ), water ( 0, 1 ) ]
    }


hallwayNS : Piece
hallwayNS =
    { name = "Hallway NS"
    , posList =
        [ wall ( -1, 0 )
        , floor ( 0, 0 )
        , wall ( 1, 0 )
        , wall ( -1, 1 )
        , floor ( 0, 1 )
        , wall ( 1, 1 )
        , wall ( -1, 2 )
        , floor ( 0, 2 )
        , wall ( 1, 2 )
        ]
    }


hallwayEW : Piece
hallwayEW =
    { name = "Hallway EW"
    , posList =
        [ wall ( 0, -1 )
        , floor ( 0, 0 )
        , wall ( 0, 1 )
        , wall ( 1, -1 )
        , floor ( 1, 0 )
        , wall ( 1, 1 )
        , wall ( 2, -1 )
        , floor ( 2, 0 )
        , wall ( 2, 1 )
        ]
    }
