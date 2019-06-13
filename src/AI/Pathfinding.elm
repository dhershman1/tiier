module AI.Pathfinding exposing (factorial, permutations, planPath)

{-| The pathfinding AI is mostly built for building out paths on the map the user can use
This approach means we can formulate a start, and exit and build multiple paths to those locations

TODO:
Finish off the decision making so it can make good choices when building out the pathways
Expand it's ability to make decisions to take more things into account.
Limit the pathing to 4 directional movement: Up, Down, Left, and Right
We want the AI to be a bit of a DM/Storyteller here so it needs to think smart

-}

import Dict exposing (Dict)
import Json.Encode as Encode


factorial : Int -> Float -> Float
factorial n current =
    if n == 1 then
        current

    else
        factorial (n - 1) (toFloat n * current)


{-| This function let's the AI see where it currently is, and where it should go next
Still a heavy WIP obviously it's a very dumb version of a simple down right path.
We don't want that, we want a nice intertwind path the player can optionally take
But also guarantee that the player can move to the next stage
-}
whichDirection : ( Int, Int ) -> ( Int, Int )
whichDirection ( x, y ) =
    if x /= 49 || y /= 49 then
        ( x + 1, y + 1 )
        -- else if x + 1 == 0 || y + 1 == 0 then
        --     ( x - 1, y - 1 )

    else
        ( x + 1, y - 1 )


planPath : ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
planPath start ( endX, endY ) current =
    let
        ( lastX, lastY ) =
            Maybe.withDefault start (List.head current)
    in
    if lastX == endX || lastY == endY then
        current

    else
        planPath start ( endX, endY ) (List.append [ whichDirection ( lastX, lastY ) ] current)



-- 137846528820
-- 137846528820


{-| Counts how many viable paths there are
(Used for debugging)
-}
permutations : Int -> Int -> Int
permutations x y =
    Debug.log "Number of permutations" (round (factorial (x + y) 1 / (factorial x 1 * factorial y 1)))
