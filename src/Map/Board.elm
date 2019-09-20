module Map.Board exposing (Board, Cell, boardToList, generate, posToString, strToTerrain, terrainToStr)

import AI.Pathfinding exposing (Position, planPath, pythagoreanCost, straightLineCost)
import Dict exposing (Dict)
import Random


type Terrain
    = Water
    | Wall
    | Floor
    | Forest
    | Abyss


type alias Point =
    ( Int, Int )


type alias Cell =
    { char : String
    , cost : Float
    , passable : Bool
    , terrain : Terrain
    , pos : Point
    }


type alias Room =
    { start : Point
    , end : Point
    }


type alias Board =
    { name : String
    , id : String
    , rooms : List Room
    , grid : Dict Point Cell
    }


fakeBoard : Board
fakeBoard =
    { name = "Test Board"
    , id = "test123"
    , rooms = []
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


terrainToStr : Cell -> String
terrainToStr { terrain } =
    case terrain of
        Wall ->
            "wall"

        Water ->
            "water"

        Floor ->
            "floor"

        Forest ->
            "forest"

        Abyss ->
            "abyss"


posToString : ( Int, Int ) -> String
posToString ( x, y ) =
    String.fromInt x ++ ", " ++ String.fromInt y


boardToList : Board -> List Cell
boardToList { grid } =
    List.map Tuple.second (Dict.toList grid)


buildBasicRoom : ( Int, Int ) -> ( Int, Int ) -> Int -> Board -> Board
buildBasicRoom coords ( endX, endY ) width board =
    let
        currentCell =
            Maybe.withDefault (Cell "~" 1 True Water coords) (Dict.get coords board.grid)

        nextBoard =
            { board | grid = Dict.insert coords (Cell "." 1 True Floor coords) board.grid }

        ( nX, nY ) =
            if Tuple.first coords < endX then
                ( Tuple.first coords + 1, Tuple.second coords )

            else
                ( Tuple.first coords - (width + 1), Tuple.second coords + 1 )
    in
    if nY > endY then
        { nextBoard | rooms = { start = coords, end = ( endX, endY ) } :: nextBoard.rooms }

    else
        buildBasicRoom ( nX, nY ) ( endX, endY ) width nextBoard


generateCells : Int -> Int -> Board -> Board
generateCells x y board =
    let
        nextBoard =
            { board | grid = Dict.insert ( x, y ) (Cell "#" 1 False Wall ( x, y )) board.grid }
    in
    if x > 0 then
        generateCells (x - 1) y nextBoard

    else
        nextBoard


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


generate : Int -> Int -> Random.Seed -> Board
generate rows cols seed =
    let
        newBoard =
            buildBasicRoom ( 30, 47 ) ( 34, 49 ) 3 (buildBasicRoom ( 0, 0 ) ( 4, 2 ) 3 (generateRow (rows - 1) (cols - 1) fakeBoard))
    in
    newBoard
