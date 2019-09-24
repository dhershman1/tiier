module Map.Board exposing (Board, Cell, boardToList, generate, posToString)

import AI.Pathfinding exposing (Position, planPath, pythagoreanCost, straightLineCost)
import Debug exposing (log)
import Dict exposing (Dict)
import Map.Tile as Tile exposing (Tile)
import Random
import Set exposing (Set)


{-| Might be worth looking into an algorithm that will let us keep the rooms from building ontop of each other TOO much so that we don't just end up with giant rooms of a dungeon

TODO:

  - Add in the ability to Generate Water terrain within the map
  - Improve the closest room finder function
  - Add setup to find a spot to place the Exit

-}
type alias Point =
    ( Int, Int )


type alias Cell =
    { char : String
    , cost : Float
    , passable : Bool
    , terrain : Tile
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


wall : Point -> Cell
wall pos =
    Cell "#" (1 / 0) False Tile.Wall pos


floor : Point -> Cell
floor pos =
    Cell "." 1 True Tile.Floor pos


water : Point -> Cell
water pos =
    Cell "~" 1 True Tile.Water pos


posToString : Point -> String
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
            { board | grid = Dict.insert ( x, y ) (Cell "" (1 / 0) False Tile.Abyss ( x, y )) board.grid }
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


getCell : Point -> Board -> Cell
getCell coords board =
    Maybe.withDefault (Cell "" (0 / 1) False Tile.Abyss coords) (Dict.get coords board.grid)


{-| Simple algo built to determine if a wall should be placed or not
-}
placeWall : Point -> Board -> Bool
placeWall ( x, y ) board =
    let
        currCell =
            getCell ( x, y ) board

        points =
            List.foldl
                (\{ terrain } count ->
                    case terrain of
                        Tile.Abyss ->
                            count - 1

                        Tile.Floor ->
                            count + 5

                        _ ->
                            count
                )
                0
                [ getCell ( x + 1, y ) board
                , getCell ( x - 1, y ) board
                , getCell ( x, y + 1 ) board
                , getCell ( x, y - 1 ) board
                , getCell ( x + 1, y + 1 ) board
                , getCell ( x - 1, y - 1 ) board
                , getCell ( x + 1, y - 1 ) board
                , getCell ( x - 1, y + 1 ) board
                ]
    in
    if currCell.terrain /= Tile.Abyss || points < 3 then
        False

    else
        True


generateWallCells : Point -> Board -> Board
generateWallCells ( x, y ) board =
    let
        nextBoard =
            if placeWall ( x, y ) board then
                { board | grid = Dict.insert ( x, y ) (Cell "#" (1 / 0) False Tile.Wall ( x, y )) board.grid }

            else
                board
    in
    if x > 0 then
        generateWallCells ( x - 1, y ) nextBoard

    else
        nextBoard


buildWalls : Point -> Board -> Board
buildWalls ( x, y ) board =
    let
        nextBoard =
            generateWallCells ( x, y ) board
    in
    if y > 0 then
        buildWalls ( x, y - 1 ) nextBoard

    else
        nextBoard


{-| Builds out a basic square room from the top right corner to the bottom right corner and places it on the map
-}
buildBasicRoom : Point -> Point -> Int -> Board -> Board
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
        { nextBoard | rooms = { start = coords, end = ( endX, endY ) } :: nextBoard.rooms }

    else
        buildBasicRoom ( nX, nY ) ( endX, endY ) width nextBoard


drawPath : List Point -> Board -> Board
drawPath path board =
    let
        coord =
            Maybe.withDefault ( 1, 1 ) (List.head path)

        rest =
            Maybe.withDefault [] (List.tail path)

        nextBoard =
            { board | grid = Dict.insert coord (floor coord) board.grid }
    in
    if List.length path == 0 && List.isEmpty rest then
        nextBoard

    else
        drawPath rest nextBoard


findClosestRoom : List Point -> Point -> Point
findClosestRoom rooms coords =
    List.foldr
        (\( x, y ) ( currX, currY ) ->
            if x > currX && y > currY then
                ( currX, currY )

            else
                ( x, y )
        )
        coords
        rooms


connectRooms : List Point -> Point -> Board -> Board
connectRooms rooms lastCoord board =
    let
        coords =
            Maybe.withDefault ( 1, 1 ) (List.head rooms)

        rest =
            Maybe.withDefault [] (List.tail rooms)

        path =
            Maybe.withDefault [] (planPath pythagoreanCost (movesFrom board) lastCoord (findClosestRoom rooms coords))

        nextBoard =
            drawPath path board
    in
    if List.isEmpty rest then
        nextBoard

    else
        connectRooms rest coords nextBoard


{-| It's important to know that most of these hardcoded numbers will probably become dynamic since this will probably be all stored in a database
-}
planRooms : Int -> Point -> Point -> List Point -> Board -> Random.Seed -> Board
planRooms maxRooms ( w1, w2 ) ( h1, h2 ) tilesList board seed =
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
            buildBasicRoom ( x - 1, y - 1 ) ( clamp 3 34 (x + width), clamp 3 49 (y + height) ) width board

        currentRooms =
            List.append tilesList [ ( clamp 3 34 (x + width), clamp 3 49 (y + height) ) ]
    in
    if maxRooms == 0 then
        connectRooms (List.append tilesList [ ( 30, 47 ) ]) ( 4, 2 ) nextBoard

    else
        planRooms (maxRooms - 1) ( w1, w2 ) ( h1, h2 ) currentRooms nextBoard nextSeed


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


{-| Can be used for the Dijkstra Algorithm for path finding, hopefully. This should find the cheapest and passable neighbor for the algo to use.
-}
lowestNeighbor : Board -> List Point -> { pos : Point, cost : Float }
lowestNeighbor board neighbors =
    let
        cheapestPoint =
            List.foldl
                (\a b ->
                    let
                        tileA =
                            Maybe.withDefault (wall a) (Dict.get a board.grid)

                        tileB =
                            Maybe.withDefault (wall b) (Dict.get b board.grid)
                    in
                    if tileA.passable && tileB.passable then
                        if tileA.cost > tileB.cost then
                            tileB.pos

                        else
                            tileA.pos

                    else if tileA.passable then
                        tileA.pos

                    else
                        tileB.pos
                )
                ( 1, 1 )
                neighbors

        { pos, cost } =
            Maybe.withDefault (floor ( 1, 1 )) (Dict.get cheapestPoint board.grid)
    in
    { pos = pos, cost = cost }


{-| The primary functionality that will plan out rooms for the board/map and build out based on the information provided
-}
generate : Int -> Int -> Random.Seed -> Board
generate rows cols seed =
    let
        boardWithEnd =
            buildBasicRoom ( 1, 1 ) ( 5, 3 ) 3 (generateRow (rows - 1) (cols - 1) fakeBoard)
    in
    buildWalls ( 34, 49 ) (planRooms 15 ( 3, 6 ) ( 3, 6 ) [] boardWithEnd seed)
