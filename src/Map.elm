module Map exposing (Biome, Map)

import Cell exposing (Cell)
import Color exposing (Color)
import Grid exposing (Grid)
import Random


type Biome
    = Forest
    | Frozen
    | Desert
    | Plains
    | Cave


type Size
    = Tiny
    | Small
    | Normal
    | Large
    | Huge


type Map
    = Map
        { name : String
        , height : Int
        , width : Int
        , biome : Biome
        , size : Size
        , grid : Grid
        , dungeon : Bool
        }


createRooms : Grid -> Grid
createRooms g =
    g


create : Int -> Int -> { name : String, biome : Biome, size : Size, dungeon : Bool } -> Map
create w h info =
    Map
        { name = info.name
        , height = h
        , width = w
        , biome = info.biome
        , size = info.size
        , grid = Grid.initialize w h
        , dungeon = info.dungeon
        }
