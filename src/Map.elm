module Map exposing (Biome, Map)

import Color exposing (Color)
import Grid exposing (Grid)
import Random



-- Each Map, and Room will be generated based off a Seed that seed will be stored within the model to load a map


type Biome
    = Forest
    | Frozen
    | Desert
    | Plains
    | Cave


type alias Map =
    { name : String
    , height : Int
    , width : Int
    , biome : Biome
    , rooms : List String
    , dungeon : Bool
    }



-- We can make a list of maps (this can be pulled from a DB later on)
-- This can then be mapped through to generate a list of rooms for each map


maps : List Map
maps =
    [ { name = "Tutorial World"
      , height = 20
      , width = 20
      , biome = Forest
      , rooms = []
      , dungeon = False
      }
    ]
