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


{-| Place the Starting Room for the map
-}
placeStartArea : ( Int, Int ) -> Board -> Board
placeStartArea current board =
    let
        currentCell =
            Maybe.withDefault (Cell "~" True Water current) (Dict.get current board.grid)

        nextBoard =
            if currentCell.terrain == Water then
                { board | grid = Dict.insert current (Cell "." True Floor current) board.grid }

            else
                board

        ( nX, nY ) =
            if Tuple.first current < 4 then
                ( Tuple.first current + 1, Tuple.second current )

            else
                ( 0, Tuple.second current + 1 )
    in
    if nY > 2 then
        nextBoard

    else
        placeStartArea ( nX, nY ) nextBoard


generate : Int -> Int -> Random.Seed -> Board
generate rows cols seed =
    placeStartArea ( 0, 0 ) (generateRow (rows - 1) (cols - 1) fakeBoard)
