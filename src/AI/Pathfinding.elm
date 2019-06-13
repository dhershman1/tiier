module AI.Pathfinding exposing (factorial, permutations)

{-| The pathfinding AI is mostly built for building out paths on the map the user can use
This approach means we can formulate a start, and exit and build multiple paths to those locations
-}

import Json.Encode as Encode


factorial : Int -> Float -> Float
factorial n current =
    if n == 1 then
        current

    else
        factorial (n - 1) (toFloat n * current)



-- 137846528820
-- 137846528820


{-| Counts how many viable paths there are
(Used for debugging)
-}
permutations : Int -> Int -> Int
permutations x y =
    Debug.log "Number of permutations" (round (factorial (x + y) 1 / (factorial x 1 * factorial y 1)))
