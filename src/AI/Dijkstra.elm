module AI.Dijkstra exposing (Point)

import Dict exposing (Dict)
import Set exposing (Set)


{-| The idea is that the Dijkstra Maps Algorithm will ultimately replace the Pathfinding algorithm in the long run
This algo will work far better for the AI to properly navigate NPCs through the randomly generated Maps and
structures of Tiier. Though the A\* algorithm may stick around to be used to connect rooms together for each map
However that may also be unneeded as I plan to move over to a crawl style of map generation instead
of the random point generation it is using now.
-}
type alias Point =
    ( Int, Int )


type alias Path =
    Dict Point { x : Int, y : Int, prev : Point }


type alias Rank =
    Int


type alias WeightedPoint =
    { x : Int
    , y : Int
    , val : Rank
    }


type alias Map =
    Dict ( Int, Int ) Rank


{-| This is a staic map size since all maps in Tiier will be the same size we don't need to worry
-}
mapSize : { x : List Int, y : List Int }
mapSize =
    { x = List.range 0 34, y = List.range 0 49 }


{-| Get the neighboring points of the current point
-}
getNeighbors : Point -> List Point
getNeighbors ( x, y ) =
    [ ( x + 1, y )
    , ( x - 1, y )
    , ( x, y + 1 )
    , ( x, y - 1 )
    ]



-- [ WeightedPoint pX y (def (Dict.get ( pX, y ) board))
-- , WeightedPoint mX y (def (Dict.get ( mX, y ) board))
-- , WeightedPoint x pY (def (Dict.get ( x, pY ) board))
-- , WeightedPoint x mY (def (Dict.get ( x, mY ) board))
-- ]


{-| Builds out a blank Dijkstra Map
-}
blankDMap : Point -> Map -> Map
blankDMap ( x, y ) board =
    let
        nextBoard =
            Dict.insert ( x, y ) 999 board

        ( nX, nY ) =
            if x < 34 then
                ( x + 1, y )

            else
                ( 0, y + 1 )
    in
    if nY < 49 then
        blankDMap ( nX, nY ) nextBoard

    else
        nextBoard



-- calc : (List Point -> { pos : Point, cost : Float }) -> Point -> Map -> Map
-- calc lowestNeighbor p board =
-- calc : (List Point -> { pos : Point, cost : Float }) -> (Point -> Bool) -> Point -> Map -> Map
-- calc getLowestNeighbor isPassable p board =
--     let
--         currBoard =
--             Dict.insert p 0 board
--     in
-- lowestNeighbor : Point -> Map -> WeightedPoint
-- lowestNeighbor p board =
--     let
--         neighbors =
--             getNeighbors p board
--     in
-- calc : (List Point -> { pos : ( Int, Int ), cost : Float }) -> Point -> Map -> Map
-- calc getLowestNeighbor p board =
--     let
--         { pos, cost } =
--             getLowestNeighbor (getNeighbors p)
--     in
--     if
