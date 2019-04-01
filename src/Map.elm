module Map exposing (Map, Room)

import Grid exposing (Grid)



-- Each Map, and Room will be generated based off a Seed that seed will be stored within the model to load a map


type alias Room =
    { id : String
    , height : Int
    , width : Int
    }


type alias Map =
    { name : String
    , id : String
    , height : Int
    , width : Int
    , biome : String
    , rooms : List Room
    , dungeon : Bool
    , grid : Grid
    }


generateRoom : Int -> Int -> Room
generateRoom x y =
    { id = "abc123"
    , height = 20
    , width = 20
    }
