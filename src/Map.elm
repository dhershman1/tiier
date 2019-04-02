module Map exposing (Map)

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
