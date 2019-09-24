module Map.Room exposing (Room, basicRoom, polyRoom)

import Dict exposing (Dict)
import Map.Tile as Tile exposing (Tile)


type alias Cell =
    { char : String
    , cost : Float
    , passable : Bool
    , terrain : Tile
    , pos : Point
    }


type alias Point =
    ( Int, Int )


type alias Feature =
    { name : String }


{-| A basic room to tell the codebase where each unique feature is of said room
-}
type alias Room =
    { start : Point
    , end : Point
    , entrence : Point
    , features : Dict Point Feature
    , grid : Dict Point Cell
    }


{-| Your basic square style room
-}
basicRoom : Point -> Point -> List Feature -> Room -> Room
basicRoom start end features room =
    Room start end ( 0, 0 ) Dict.empty Dict.empty


{-| More complex rooms with more polygonal shapes
-}
polyRoom : Point -> Point -> Maybe (List Feature) -> Room
polyRoom start end features =
    Room start end ( 0, 0 ) Dict.empty Dict.empty
