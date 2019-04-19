module Map exposing (Map, empty)

import Biome exposing (Biome)
import Cell exposing (Cell)
import Color exposing (Color)
import Grid exposing (Grid)
import Random


type Map
    = Map
        { name : String
        , height : Int
        , width : Int
        , biome : Biome
        , grid : Grid
        , dungeon : Bool
        }


empty : Map
empty =
    Map
        { name = ""
        , height = 0
        , width = 0
        , biome = Biome.fromString ""
        , grid = Grid.empty
        , dungeon = False
        }


createRooms : Grid -> Grid
createRooms g =
    g


create : Int -> Int -> { name : String, biome : String, dungeon : Bool } -> Map
create w h info =
    Map
        { name = info.name
        , height = h
        , width = w
        , biome = Biome.fromString info.biome
        , grid = Grid.initialize w h
        , dungeon = info.dungeon
        }
