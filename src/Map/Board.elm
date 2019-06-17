module Map.Board exposing (Board, boardToList, generate, posToString, strToTerrain, terrainToClass)

import Dict exposing (Dict)
import Map.Pieces exposing (..)
import Random


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
            { board | grid = Dict.insert ( x, y ) (Cell "~" True Water ( x, y )) board.grid }
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



-- withinBounds : Int -> Int -> Int -> Bool
-- withinBounds coord min max =


buildBasicRoom : ( Int, Int ) -> ( Int, Int ) -> Int -> Int -> Board -> Board
buildBasicRoom coords ( endX, endY ) width height board =
    let
        currentCell =
            Maybe.withDefault (Cell "~" True Water coords) (Dict.get coords board.grid)

        nextBoard =
            -- { board | grid = Dict.insert coords (Cell "." True Floor coords) board.grid }
            if currentCell.terrain == Water then
                { board | grid = Dict.insert coords (Cell "." True Floor coords) board.grid }

            else
                board

        ( nX, nY ) =
            if Tuple.first coords < endX then
                ( Tuple.first coords + 1, Tuple.second coords )

            else
                ( Tuple.first coords - width, Tuple.second coords + 1 )
    in
    if nY == endY then
        nextBoard

    else
        buildBasicRoom ( nX, nY ) ( endX, endY ) width height nextBoard


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
            buildBasicRoom ( x, y ) ( clamp 3 34 (x + width), clamp 3 49 (y + height) ) width height board
    in
    if maxRooms == 0 then
        nextBoard

    else
        planRooms (maxRooms - 1) ( w1, w2 ) ( h1, h2 ) nextBoard nextSeed


generate : Int -> Int -> Random.Seed -> Board
generate rows cols seed =
    planRooms 20 ( 3, 6 ) ( 3, 6 ) (generateRow (rows - 1) (cols - 1) fakeBoard) seed



-- buildBasicRoom ( 0, 0 ) 3 2 (generateRow (rows - 1) (cols - 1) fakeBoard)
