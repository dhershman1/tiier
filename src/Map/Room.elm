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


findCorners : Room -> { topLeft : Point, topRight : Point, bottomLeft : Point, bottomRight : Point }
findCorners { start, end } =
    let
        ( startX, startY ) =
            log "start" start

        ( endX, endY ) =
            log "end" end
    in
    { topLeft = ( startX + 1, startY + 1 )
    , topRight = ( endX + 1 - startX, startY + 1 )
    , bottomLeft = ( startX + 1, endY + 1 - startY )
    , bottomRight = ( endX + 1, endY + 1 )
    }


{-| Picks a random wall and places a door there
-}
placeDoor : Random.Seed -> Room -> Room
placeDoor seed room =
    let
        { topLeft, topRight, bottomLeft, bottomRight } =
            log "Corners" <| findCorners room

        ( startX, startY ) =
            room.start

        ( endX, endY ) =
            room.end

        ( direction, newSeed ) =
            Random.step (Random.uniform North [ South, East, West ]) seed
    in
    case direction of
        North ->
            let
                ( x, _ ) =
                    Random.step (Random.int (Tuple.second topLeft) (Tuple.second topRight)) newSeed
            in
            { room | grid = Dict.insert ( x, startY ) (Tile.door ( x, startY )) room.grid }

        South ->
            let
                ( x, _ ) =
                    Random.step (Random.int (Tuple.second bottomLeft) (Tuple.second bottomRight)) newSeed
            in
            { room | grid = Dict.insert ( x, endY ) (Tile.door ( x, endY )) room.grid }

        East ->
            let
                ( y, _ ) =
                    Random.step (Random.int (Tuple.first topRight) (Tuple.first bottomRight)) newSeed
            in
            { room | grid = Dict.insert ( endX, y ) (Tile.door ( endX, y )) room.grid }

        West ->
            let
                ( y, _ ) =
                    Random.step (Random.int (Tuple.first topLeft) (Tuple.first bottomLeft)) newSeed
            in
            { room | grid = Dict.insert ( startX, y ) (Tile.door ( startX, y )) room.grid }


generateSeed : Room -> Random.Seed
generateSeed room =
    Random.initialSeed (Tuple.first room.start + Tuple.second room.start + Tuple.first room.end + Tuple.second room.end)


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
