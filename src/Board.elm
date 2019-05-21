module Board exposing (Board, Cell, Terrain, boardToList, empty, generate, posToString, terrainToClass)

import Color exposing (Color)
import Debug exposing (log)
import Dict exposing (Dict)
import Messages exposing (Msg)
import Random


type Neighbor
    = North
    | South
    | East
    | West


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



-- type Cell
--     = Cell CellRec


type alias Board =
    { name : String
    , biome : String
    , id : String
    , grid : Dict ( Int, Int ) Cell
    , dungeon : Bool
    }



-- type Board
--     = Board
--         { name : String
--         , biome : String
--         , id : String
--         , grid : Dict String Cell
--         , dungeon : Bool
--         }


fakeBoard : Board
fakeBoard =
    { name = "Test Board"
    , id = "test123"
    , biome = "Forest"
    , dungeon = False
    , grid = Dict.empty
    }


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


posFromString : String -> Maybe ( Int, Int )
posFromString s =
    case String.split ", " s of
        x :: y :: [] ->
            let
                newX =
                    case String.toInt x of
                        Nothing ->
                            0

                        Just iX ->
                            iX

                newY =
                    case String.toInt y of
                        Nothing ->
                            0

                        Just iY ->
                            iY
            in
            Just <| ( newX, newY )

        _ ->
            Nothing


emptyCell : Cell
emptyCell =
    { char = ""
    , passable = False
    , terrain = Abyss
    , pos = ( 0, 0 )
    }


empty : Board
empty =
    { name = ""
    , id = ""
    , biome = ""
    , grid = Dict.empty
    , dungeon = False
    }


boardToList : Board -> List Cell
boardToList { grid } =
    List.map Tuple.second (Dict.toList grid)


getTerrain : ( Int, Int ) -> Dict String Cell -> Terrain
getTerrain pos grid =
    case Maybe.withDefault emptyCell (Dict.get (posToString pos) grid) of
        { terrain } ->
            terrain



-- getNeighbors : ( Int, Int ) -> Board -> List Cell
-- getNeighbors ( x, y ) { grid } =
--     List.map (\pos -> Maybe.withDefault emptyCell (Dict.get (posToString pos) grid))
--         [ ( x - 3, y )
--         , ( x - 2, y )
--         , ( x - 1, y )
--         , ( x, y + 1 )
--         , ( x, y + 2 )
--         , ( x, y + 3 )
--         ]
-- getNeighborTerrain : ( Int, Int ) -> Board -> List Terrain
-- getNeighborTerrain ( x, y ) { grid } =
--     List.map (\oldPos -> getTerrain oldPos grid)
--         [ ( x - 3, y )
--         , ( x - 2, y )
--         , ( x - 1, y )
--         , ( x, y + 1 )
--         , ( x, y + 2 )
--         , ( x, y + 3 )
--         ]


count : List Terrain -> Terrain -> Int
count terrList t =
    List.partition (\lt -> lt == t) terrList
        |> Tuple.first
        |> List.length



-- getWeight : ( Int, Int ) -> Terrain -> Board -> Float
-- getWeight pos t board =
--     let
--         neighborsTerrain =
--             getNeighborTerrain pos board
--         matchedNeighbors =
--             count (log "neighborsTerrain" neighborsTerrain) t
--     in
--     case matchedNeighbors of
--         6 ->
--             1
--         5 ->
--             10
--         4 ->
--             20
--         3 ->
--             25
--         2 ->
--             30
--         1 ->
--             35
--         _ ->
--             0


generateOneOf : ( Int, Int ) -> Random.Seed -> ( Cell, Random.Seed )
generateOneOf pos seed =
    Random.step
        (Random.uniform (Cell "#" False Wall pos)
            [ Cell "~" True Water pos
            , Cell "." True Floor pos
            , Cell "!" True Forest pos
            , Cell "=" True TownRoad pos
            , Cell "" False Abyss pos
            ]
        )
        seed



-- randoCell : ( Int, Int ) -> Board -> Random.Seed -> ( Cell, Random.Seed )
-- randoCell pos board seed =
--     Random.step
--         (Random.weighted
--             ( getWeight pos Wall board, Cell "#" False Wall pos )
--             [ ( getWeight pos Water board, Cell "~" True Water pos )
--             , ( getWeight pos Floor board, Cell "." True Floor pos )
--             , ( getWeight pos Forest board, Cell "!" True Forest pos )
--             , ( getWeight pos TownRoad board, Cell "=" True TownRoad pos )
--             , ( getWeight pos Abyss board, Cell "" False Abyss pos )
--             ]
--         )
--         seed


generateCell : Int -> Int -> Board -> Random.Seed -> Board
generateCell x y board seed =
    let
        ( nextCell, nextSeed ) =
            generateOneOf ( x, y ) seed

        nextBoard =
            { board | grid = Dict.insert ( x, y ) (Cell "~" True Water ( x, y )) board.grid }
    in
    if x > 0 then
        generateCell (x - 1) y nextBoard nextSeed

    else
        nextBoard


generateRow : Int -> Int -> Board -> Random.Seed -> Board
generateRow x y board seed =
    let
        nextBoard =
            generateCell x y board seed
    in
    if y > 0 then
        generateRow x (y - 1) nextBoard seed

    else
        nextBoard


getSize : Random.Seed -> ( Int, Random.Seed )
getSize seed =
    Random.step (Random.int 5 10) seed



-- Random.step (Random.map2 (\x y -> ( x, y )) (Random.int 5 15) (Random.int 5 15)) seed


getPos : Int -> Int -> Random.Seed -> ( ( Int, Int ), Random.Seed )
getPos x y seed =
    Random.step (Random.pair (Random.int 15 x) (Random.int 15 y)) seed


expandRoom : Int -> ( Int, Int ) -> Board -> Board
expandRoom size ( posX, posY ) board =
    let
        newPos =
            ( posX - 1, posY - 1 )

        nextBoard =
            { board | grid = Dict.insert newPos (Cell "." True Floor newPos) board.grid }
    in
    if size > 0 then
        expandRoom (size - 1) newPos nextBoard

    else
        nextBoard


generateRooms : Int -> Int -> Int -> Random.Seed -> Board -> Board
generateRooms x y roomCount seed board =
    let
        ( pos, posSeed ) =
            getPos x y seed

        ( size, nextSeed ) =
            getSize seed

        nextBoard =
            { board | grid = Dict.insert pos (Cell "." True Floor pos) board.grid }
    in
    if roomCount < 0 then
        generateRooms x y (roomCount + 1) nextSeed nextBoard

    else
        nextBoard


{-| The Board will be pulled from our DB to get its stats like Biome, name, dungeon, etc.
For now though we can also just fake that. Replace "fakeBoard" with an actual db return
-}
generate : Int -> Int -> Random.Seed -> Board
generate width height seed =
    -- generateRow (width - 1) (height - 1) fakeBoard seed
    generateRooms (width - 1) (height - 1) 0 seed (generateRow (width - 1) (height - 1) fakeBoard seed)
