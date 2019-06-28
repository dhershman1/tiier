module Map.Board exposing (Board, Cell, boardToList, generate, posToString, strToTerrain, terrainToClass)

import AI.Pathfinding exposing (Position, planPath, pythagoreanCost, straightLineCost)
import Dict exposing (Dict)
import Random
import Set exposing (Set)


{-| TODO:

  - Each room & Path needs to get a wall built around it BEFORE we attach them together with the paths.
  - Transition from a Random point generator for the dungeon into more of a crawl generation for the dungeons
  - Improve the cost calculations for connecting rooms
  - Add Database pulling for dungeon data

-}
type Terrain
    = Water
    | ShallowWater
    | Wall
    | Floor
    | Forest
    | TownRoad
    | Abyss


type alias Point =
    ( Int, Int )


type alias Cell =
    { char : String
    , cost : Float
    , passable : Bool
    , terrain : Terrain
    , pos : ( Int, Int )
    }


type alias Room =
    { sizeX : Int
    , sizeY : Int
    , grid : Dict ( Int, Int ) Cell
    }


type alias Board =
    { name : String
    , id : String
    , rooms : List Room
    , grid : Dict ( Int, Int ) Cell
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

        "shallow-water" ->
            ShallowWater

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

        ShallowWater ->
            "shallow-water"

        Abyss ->
            "abyss"


wall : Point -> Cell
wall pos =
    Cell "#" (1 / 0) False Wall pos


floor : Point -> Cell
floor pos =
    Cell "." 1 True Floor pos


water : Point -> Cell
water pos =
    Cell "~" 1 True Water pos


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
            { board | grid = Dict.insert ( x, y ) (Cell "" (1 / 0) False Abyss ( x, y )) board.grid }
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
            Maybe.withDefault (water coords) (Dict.get coords board.grid)

        nextBoard =
            { board | grid = Dict.insert coords (floor coords) board.grid }

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


drawPath : List ( Int, Int ) -> Board -> Board
drawPath path board =
    let
        coord =
            Maybe.withDefault ( 0, 0 ) (List.head path)

        rest =
            Maybe.withDefault [] (List.tail path)

        nextBoard =
            { board | grid = Dict.insert coord (floor coord) board.grid }
    in
    if List.length path == 0 && List.isEmpty rest then
        nextBoard

    else
        drawPath rest nextBoard


{-| The primary functionality that will plan out rooms for the board/map and build out based on the information provided
-}
generate : Int -> Int -> Random.Seed -> Board
generate rows cols seed =
    let
        boardStart =
            buildBasicRoom ( 0, 0 ) ( 4, 2 ) 3 (generateRow (rows - 1) (cols - 1) fakeBoard)
    in
    boardStart



-- planRooms 10 ( 3, 6 ) ( 3, 6 ) [] boardWithEnd seed
-- buildBasicRoom ( 0, 0 ) 3 2 (generateRow (rows - 1) (cols - 1) fakeBoard)
