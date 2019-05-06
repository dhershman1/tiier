module Board exposing (Board, empty)

import Color exposing (Color)
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
    | RoomCore
    | Abyss


type alias CellRec =
    { char : String
    , passable : Bool
    , terrain : Terrain
    , pos : ( Int, Int )
    }


type Cell
    = Cell CellRec


type Board
    = Board
        { name : String
        , biome : String
        , id : String
        , grid : Dict String Cell
        , dungeon : Bool
        }


fakeBoard : { name : String, biome : String, id : String, dungeon : Bool }
fakeBoard =
    { name = "Test Board"
    , id = "test123"
    , biome = "Forest"
    , dungeon = False
    }


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


getName : Board -> String
getName (Board { name }) =
    name


emptyCell : Cell
emptyCell =
    Cell
        { char = "."
        , passable = False
        , terrain = Abyss
        , pos = ( 0, 0 )
        }


empty : Board
empty =
    Board
        { name = ""
        , id = ""
        , biome = ""
        , grid = Dict.empty
        , dungeon = False
        }


getTerrain : ( Int, Int ) -> Dict String Cell -> Terrain
getTerrain pos grid =
    case Maybe.withDefault emptyCell (Dict.get (posToString pos) grid) of
        Cell { terrain } ->
            terrain


getNeighborTerrain : ( Int, Int ) -> Board -> List Terrain
getNeighborTerrain ( x, y ) (Board { grid }) =
    List.map (\oldPos -> getTerrain oldPos grid)
        [ ( x - 3, y )
        , ( x - 2, y )
        , ( x - 1, y )
        , ( x, y + 1 )
        , ( x, y + 2 )
        , ( x, y + 3 )
        ]


generateOneOf : ( Int, Int ) -> ( Cell, List Cell )
generateOneOf pos =
    ( Cell (CellRec "#" False Wall pos)
    , [ Cell (CellRec "~" True Water pos)
      , Cell (CellRec "." True Floor pos)
      , Cell (CellRec "!" True Forest pos)
      , Cell (CellRec "=" True TownRoad pos)
      , Cell (CellRec "." True RoomCore pos)
      , Cell (CellRec "" False Abyss pos)
      ]
    )



-- validateNeighbors : List Cell -> List Cell -> Neighbor -> ( Cell, List Cell )
-- validateNeighbors possibleTiles possibleNeighbors neighborDirection =
--     let
--         filteredPossibilities =
--             List.filter (\neigh -> List.any (\self -> validJunction neighborDirection self neigh) possibleTiles) possibleNeighbors
--     in
--     case filteredPossibilities of
--         t :: others ->
--             ( t, others )
--         [] ->
--             ( Cell "" True Floor ( 0, 0 ), [] )


{-| The Board will be pulled from our DB to get its stats like Biome, name, dungeon, etc.
For now though we can also just fake that. Replace "fakeBoard" with an actual db return
-}
generate : Random.Seed -> String -> Board
generate seed id =
    empty
