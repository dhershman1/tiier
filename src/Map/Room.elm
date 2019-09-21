module Map.Room exposing (Room, create, empty)

import Dict exposing (Dict)
import Map.Terrain as Terrain exposing (Terrain)
import Random


type alias Cell =
    { char : String
    , cost : Float
    , passable : Bool
    , terrain : Terrain
    , pos : Point
    }


type alias Point =
    ( Int, Int )


type Room
    = Room
        { start : Point
        , end : Point
        , seed : Random.Seed
        , grid : Dict Point Cell
        }


combineCoords : Point -> Point -> Int
combineCoords ( x, y ) ( x2, y2 ) =
    x + y + x2 + y2


empty : Room
empty =
    Room
        { start = ( 0, 0 )
        , end = ( 0, 0 )
        , seed = Random.initialSeed 0
        , grid = Dict.empty
        }


buildRoom : Point -> Point -> Room -> Room
buildRoom ( x, y ) ( endX, endY ) room =
    room


create : Point -> Point -> Room
create start end =
    Room
        { start = start
        , end = end
        , seed = Random.initialSeed (combineCoords start end)
        , grid = Dict.empty
        }
