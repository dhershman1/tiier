module Map exposing (Map)

import Grid exposing (Grid)



-- Each Map, and Room will be generated based off a Seed that seed will be stored within the model to load a map


type alias Map =
    { name : String
    , seed : String
    , height : Int
    , width : Int
    , biome : String
    , rooms : List String
    , dungeon : Bool
    }
