module Map.Room exposing (Room, basicRoom, create, empty, polyRoom)

import Debug exposing (log)
import Dict exposing (Dict)
import Map.Tile as Tile exposing (Tile)
import Random


type alias Point =
    ( Int, Int )


type Direction
    = North
    | South
    | East
    | West


type alias Feature =
    { name : String }


{-| A basic room to tell the codebase where each unique feature is of said room
-}
type alias Room =
    { start : Point
    , end : Point
    , entrence : Point
    , features : Dict Point Feature
    , grid : Dict Point Tile.Cell
    }


empty : Room
empty =
    { start = ( 0, 0 )
    , end = ( 0, 0 )
    , entrence = ( 0, 0 )
    , features = Dict.empty
    , grid = Dict.empty
    }


create : Point -> Point -> Room
create start end =
    { start = start
    , end = end
    , entrence = ( Tuple.first end + 1, Tuple.second end + 1 )
    , features = Dict.empty
    , grid = Dict.empty
    }


area : Room -> ( Point, Point )
area { start, end } =
    let
        ( startX, startY ) =
            start

        ( endX, endY ) =
            end
    in
    ( ( startX, endX ), ( startY, endY ) )


{-| Your basic square style room
-}
basicRoom : Point -> Point -> Int -> List Feature -> Room -> Room
basicRoom coords ( endX, endY ) width features room =
    let
        nextRoom =
            { room | grid = Dict.insert coords (Tile.floor coords) room.grid }

        ( nX, nY ) =
            if Tuple.first coords < endX then
                ( Tuple.first coords + 1, Tuple.second coords )

            else
                ( Tuple.first coords - (width + 1), Tuple.second coords + 1 )
    in
    if nY > endY then
        nextRoom

    else
        basicRoom ( nX, nY ) ( endX, endY ) width features nextRoom


{-| More complex rooms with more polygonal shapes
-}
polyRoom : Point -> Point -> Maybe (List Feature) -> Room
polyRoom start end features =
    Room start end ( 0, 0 ) Dict.empty Dict.empty
