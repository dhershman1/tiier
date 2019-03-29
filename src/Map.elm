module Map exposing (Map, Room)


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
    }


generateRoom : Int -> Int -> Room
generateRoom x y =
    { id = "abc123"
    , height = 20
    , width = 20
    }
