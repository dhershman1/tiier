module Map.Board exposing (Board, boardToList, generate, posToString, strToTerrain, terrainToClass)

import AI.Pathfinding exposing (Position, findPath)
import Dict exposing (Dict)
import Random
import Set exposing (Set)


type Terrain
    = Water
    | Wall
    | Floor
    | Forest
    | TownRoad
    | Abyss


type alias Cell =
    { char : String
    , passable : Bool
    , terrain : Terrain
    , pos : ( Int, Int )
    }


type alias Board =
    { name : String
    , biome : String
    , id : String
    , grid : Dict ( Int, Int ) Cell
    , dungeon : Bool
    }


type alias MapStats =
    { width : Int
    , height : Int
    , x : Int
    , y : Int
    }


fakeBoard : Board
fakeBoard =
    { name = "Test Board"
    , id = "test123"
    , biome = "Forest"
    , dungeon = False
    , grid = Dict.empty
    }


strToTerrain : String -> Terrain
strToTerrain str =
    case str of
        "wall" ->
            Wall

        "water" ->
            Water

        "floor" ->
            Floor

        "forest" ->
            Forest

        _ ->
            Abyss


terrainToClass : Cell -> String
terrainToClass { terrain } =
    case terrain of
        Wall ->
            "wall"

        Water ->
            "water"

        Floor ->
            "floor"

        Forest ->
            "forest"

        TownRoad ->
            "town-road"

        Abyss ->
            "abyss"


posToString : ( Int, Int ) -> String
posToString ( x, y ) =
    String.fromInt x ++ ", " ++ String.fromInt y


boardToList : Board -> List Cell
boardToList { grid } =
    List.map Tuple.second (Dict.toList grid)


{-| Generate all of the floor cells to fill the board
-}
generateCells : Int -> Int -> Board -> Board
generateCells x y board =
    let
        nextBoard =
            { board | grid = Dict.insert ( x, y ) (Cell "#" True Wall ( x, y )) board.grid }
    in
    if x > 0 then
        generateCells (x - 1) y nextBoard

    else
        nextBoard


{-| Generate each row of the board
-}
generateRow : Int -> Int -> Board -> Board
generateRow x y board =
    let
        nextBoard =
            generateCells x y board
    in
    if y > 0 then
        generateRow x (y - 1) nextBoard

    else
        nextBoard


{-| Builds out a basic square room from the top right corner to the bottom right corner and places it on the map
-}
buildBasicRoom : ( Int, Int ) -> ( Int, Int ) -> Int -> Board -> Board
buildBasicRoom coords ( endX, endY ) width board =
    let
        currentCell =
            Maybe.withDefault (Cell "~" True Water coords) (Dict.get coords board.grid)

        nextBoard =
            { board | grid = Dict.insert coords (Cell "." True Floor coords) board.grid }

        ( nX, nY ) =
            if Tuple.first coords < endX then
                ( Tuple.first coords + 1, Tuple.second coords )

            else
                ( Tuple.first coords - (width + 1), Tuple.second coords + 1 )
    in
    if nY > endY then
        nextBoard

    else
        buildBasicRoom ( nX, nY ) ( endX, endY ) width nextBoard


{-| It's important to know that most of these hardcoded numbers will probably become dynamic since this will probably be all stored in a database
-}
planRooms : Int -> ( Int, Int ) -> ( Int, Int ) -> Board -> Random.Seed -> Board
planRooms maxRooms ( w1, w2 ) ( h1, h2 ) board seed =
    let
        ( { width, height, x, y }, nextSeed ) =
            Random.step
                (Random.map4 MapStats
                    (Random.int w1 w2)
                    (Random.int h1 h2)
                    (Random.int 3 34)
                    (Random.int 3 49)
                )
                seed

        nextBoard =
            buildBasicRoom ( x, y ) ( clamp 3 34 (x + width), clamp 3 49 (y + height) ) width board
    in
    if maxRooms == 0 then
        nextBoard

    else
        planRooms (maxRooms - 1) ( w1, w2 ) ( h1, h2 ) nextBoard nextSeed


movesFrom : Board -> Position -> Set Position
movesFrom world ( x, y ) =
    let
        results =
            Set.empty
    in
    if x == 0 && y == 0 then
        Set.union (Set.fromList [ ( 1, 0 ), ( 0, 1 ) ]) results

    else if x == 0 then
        Set.insert ( 1, y ) results

    else if y == 0 then
        Set.insert ( x, 1 ) results

    else
        Set.union (Set.fromList [ ( x + 1, y ), ( x, y + 1 ), ( x - 1, y ), ( x, y - 1 ) ]) results


{-| The primary functionality that will plan out rooms for the board/map and build out based on the information provided
-}
generate : Int -> Int -> Random.Seed -> Board
generate rows cols seed =
    let
        boardWithEnd =
            buildBasicRoom ( 30, 47 ) ( 34, 49 ) 3 (buildBasicRoom ( 0, 0 ) ( 4, 2 ) 3 (generateRow (rows - 1) (cols - 1) fakeBoard))
    in
    planRooms 30 ( 3, 6 ) ( 3, 6 ) boardWithEnd seed



-- buildBasicRoom ( 0, 0 ) 3 2 (generateRow (rows - 1) (cols - 1) fakeBoard)
