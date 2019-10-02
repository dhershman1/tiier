module Map.Board exposing (Board, empty, generate, moveCharacter, posToString, toList)

import AStar exposing (Position, findPath, straightLineCost)
import Debug exposing (log)
import Dict exposing (Dict)
import List.Extra as ListExtra
import Map.Room as Room exposing (Room)
import Map.Tile as Tile exposing (Tile)
import Random
import Random.List
import Set exposing (Set)


{-| Might be worth looking into an algorithm that will let us keep the rooms from building ontop of each other TOO much so that we don't just end up with giant rooms of a dungeon

TODO:

  - Add in the ability to Generate Water terrain within the map
  - Improve the closest room finder function
  - Add setup to find a spot to place the Entrence

-}
type alias Point =
    ( Int, Int )


type Entity
    = Character
    | Item
    | Actor
    | Trap


type alias Board =
    { name : String
    , id : String
    , rooms : Dict Int Room
    , floors : Int
    , grid : Dict Point Tile.Cell
    }


type alias MapStats =
    { width : Int
    , height : Int
    , x : Int
    , y : Int
    }


empty : Board
empty =
    { name = ""
    , id = ""
    , rooms = Dict.empty
    , floors = 0
    , grid = Dict.empty
    }


fakeBoard : Board
fakeBoard =
    { name = "Test Board"
    , id = "test123"
    , rooms = Dict.empty
    , floors = 1
    , grid = Dict.empty
    }


posToString : Point -> String
posToString ( x, y ) =
    String.fromInt x ++ ", " ++ String.fromInt y


toList : Board -> List Tile.Cell
toList { grid } =
    List.map Tuple.second (Dict.toList grid)


pointMinMax : List Point -> Point -> ( Point, Point )
pointMinMax points ( x, y ) =
    let
        xs =
            List.map Tuple.first points

        ys =
            List.map Tuple.second points
    in
    ( ( Maybe.withDefault x (List.minimum xs), Maybe.withDefault x (List.maximum xs) ), ( Maybe.withDefault y (List.minimum ys), Maybe.withDefault y (List.maximum ys) ) )


{-| Generate all of the floor cells to fill the board
-}
generateCells : Int -> Int -> Board -> Board
generateCells x y board =
    let
        nextBoard =
            { board | grid = Dict.insert ( x, y ) (Tile.abyss ( x, y )) board.grid }
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


placeEntrance : ( Random.Seed, Board ) -> ( Board, Point )
placeEntrance ( seed, board ) =
    let
        room =
            Maybe.withDefault Room.empty (Dict.get 0 board.rooms)

        ( ( point, _ ), nextSeed ) =
            Random.step (Random.List.choose (Room.pointsList room)) seed

        safePoint =
            Maybe.withDefault ( 0, 0 ) point
    in
    ( { board | grid = Dict.insert safePoint (Tile.stairsUp safePoint) board.grid }, safePoint )


placeExit : ( Random.Seed, Board ) -> ( Random.Seed, Board )
placeExit ( seed, board ) =
    let
        room =
            Maybe.withDefault Room.empty (Dict.get (Dict.size board.rooms - 2) board.rooms)

        ( ( point, _ ), nextSeed ) =
            Random.step (Random.List.choose (Room.pointsList room)) seed

        safePoint =
            Maybe.withDefault ( 0, 0 ) point
    in
    ( nextSeed, { board | grid = Dict.insert safePoint (Tile.stairsDown safePoint) board.grid } )


getCell : Point -> Board -> Tile.Cell
getCell coords board =
    Maybe.withDefault (Tile.abyss coords) (Dict.get coords board.grid)


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
    if currCell.terrain /= Tile.Abyss || points < 4 then
        False

    else
        True


generateWallCells : Point -> Board -> Board
generateWallCells ( x, y ) board =
    let
        nextBoard =
            if placeWall ( x, y ) board then
                { board | grid = Dict.insert ( x, y ) (Tile.wall ( x, y )) board.grid }

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
buildBasicRoom : Point -> Point -> Int -> Board -> Int -> Board
buildBasicRoom start end width board count =
    let
        room =
            Room.basicRoom start end width [] (Room.create start end)
    in
    { board | grid = Dict.union room.grid board.grid, rooms = Dict.insert count room board.rooms }


drawPath : List Point -> Board -> Board
drawPath path board =
    case path of
        coord :: rest ->
            let
                nextBoard =
                    { board | grid = Dict.insert coord (Tile.floor coord) board.grid }
            in
            if List.length path == 0 && List.isEmpty rest then
                nextBoard

            else
                drawPath rest nextBoard

        [] ->
            board


{-| This function should be more intelligent about finding a room right now it's pretty dumb about it
-}
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
            Maybe.withDefault [] (findPath straightLineCost (movesFrom board) lastCoord (findClosestRoom rooms coords))

        nextBoard =
            drawPath path board
    in
    if List.isEmpty rest then
        nextBoard

    else
        connectRooms rest coords nextBoard


{-| Randomly place our start room on the map
-}
placeStartRoom : Point -> Point -> Random.Seed -> Board -> Board
placeStartRoom ( w1, w2 ) ( h1, h2 ) seed board =
    let
        ( { width, height, x, y }, nextSeed ) =
            Random.step
                (Random.map4 MapStats
                    (Random.int w1 w2)
                    (Random.int h1 h2)
                    (Random.int 3 47)
                    (Random.int 3 93)
                )
                seed

        nextBoard =
            buildBasicRoom ( x - 1, y - 1 ) ( clamp 3 47 (x + width), clamp 3 93 (y + height) ) width board 99
    in
    nextBoard


{-| It's important to know that most of these hardcoded numbers will probably become dynamic since this will probably be all stored in a database
-}
planRooms : Int -> Point -> Point -> List Point -> Random.Seed -> Board -> Board
planRooms roomsLeft ( w1, w2 ) ( h1, h2 ) tilesList seed board =
    let
        ( { width, height, x, y }, nextSeed ) =
            Random.step
                (Random.map4 MapStats
                    (Random.int w1 w2)
                    (Random.int h1 h2)
                    (Random.int 3 47)
                    (Random.int 3 93)
                )
                seed

        nextBoard =
            buildBasicRoom ( x - 1, y - 1 ) ( clamp 3 47 (x + width), clamp 3 93 (y + height) ) width board roomsLeft

        currentRooms =
            List.append tilesList [ ( clamp 3 47 (x + width), clamp 3 93 (y + height) ) ]
    in
    if roomsLeft == 0 then
        connectRooms tilesList ( x - 1, y - 1 ) nextBoard

    else
        planRooms (roomsLeft - 1) ( w1, w2 ) ( h1, h2 ) currentRooms nextSeed nextBoard


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
                            Maybe.withDefault (Tile.wall a) (Dict.get a board.grid)

                        tileB =
                            Maybe.withDefault (Tile.wall b) (Dict.get b board.grid)
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
            Maybe.withDefault (Tile.abyss ( 1, 1 )) (Dict.get cheapestPoint board.grid)
    in
    { pos = pos, cost = cost }


moveCharacter : Board -> Point -> Point -> Board
moveCharacter board currCoords coords =
    let
        tmp =
            log "coords" coords

        toTile =
            log "toTile" <| Maybe.withDefault (Tile.abyss coords) (Dict.get coords board.grid)

        nextBoard =
            { board | grid = Dict.insert currCoords (Tile.floor currCoords) board.grid }
    in
    if toTile.passable then
        { nextBoard | grid = Dict.insert coords (Tile.character coords) board.grid }

    else
        board


{-| The primary functionality that will plan out rooms for the board/map and build out based on the information provided
-}
generate : Int -> Int -> Random.Seed -> ( Board, Point )
generate rows cols seed =
    -- Build And Fill Map with blank tiles
    generateRow (rows - 1) (cols - 1) fakeBoard
        -- Build the starting room
        |> placeStartRoom ( 3, 6 ) ( 3, 6 ) seed
        -- Build out the rest of the dungeon rooms
        |> planRooms 25 ( 3, 6 ) ( 3, 6 ) [] seed
        -- Wrap the dungeon within walls
        |> buildWalls ( 49, 94 )
        -- Create a seed, board tuple for our exit and entrance placer
        |> Tuple.pair seed
        -- These next two steps might be able to get combined into a single function
        -- Place our floor exit
        |> placeExit
        -- Place floor Entrance
        |> placeEntrance
