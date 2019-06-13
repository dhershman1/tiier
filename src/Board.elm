module Board exposing (Board, Cell, Terrain, boardToList, empty, generate, posToString, terrainToClass)

import AI.Pathfinding exposing (planPath)
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


count : List Terrain -> Terrain -> Int
count terrList t =
    List.partition (\lt -> lt == t) terrList
        |> Tuple.first
        |> List.length


generateOneOf : ( Int, Int ) -> Random.Seed -> ( Cell, Random.Seed )
generateOneOf pos seed =
    Random.step
        (Random.weighted ( 25, Cell "#" False Wall pos )
            [ ( 15, Cell "~" True Water pos )
            , ( 50, Cell "." True Floor pos )

            -- , Cell "!" True Forest pos
            -- , Cell "=" True TownRoad pos
            -- , Cell "" False Abyss pos
            ]
        )
        seed


generateCell : Int -> Int -> List ( Int, Int ) -> Board -> Random.Seed -> ( Board, Random.Seed )
generateCell x y path board seed =
    let
        ( nextCell, nextSeed ) =
            generateOneOf ( x, y ) seed

        nextBoard =
            if List.member ( x, y ) path then
                { board | grid = Dict.insert ( x, y ) (Cell "." True Floor ( x, y )) board.grid }

            else
                { board | grid = Dict.insert ( x, y ) (Cell "~" True Water ( x, y )) board.grid }
    in
    if x > 0 then
        generateCell (x - 1) y path nextBoard nextSeed

    else
        ( nextBoard, nextSeed )


generateRow : Int -> Int -> List ( Int, Int ) -> Board -> Random.Seed -> Board
generateRow x y path board seed =
    let
        ( nextBoard, nextSeed ) =
            generateCell x y path board seed
    in
    if y > 0 then
        generateRow x (y - 1) path nextBoard nextSeed

    else
        nextBoard


getSize : Random.Seed -> ( Int, Random.Seed )
getSize seed =
    Random.step (Random.int 5 10) seed


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
    if roomCount <= 0 then
        generateRooms x y (roomCount + 1) nextSeed nextBoard

    else
        nextBoard


{-| The Board will be pulled from our DB to get its stats like Biome, name, dungeon, etc.
For now though we can also just fake that. Replace "fakeBoard" with an actual db return
-}
generate : Int -> Int -> Random.Seed -> Board
generate width height seed =
    -- generateRow (width - 1) (height - 1) fakeBoard seed
    generateRow (width - 1) (height - 1) (planPath ( 0, 0 ) ( 49, 49 ) []) fakeBoard seed
