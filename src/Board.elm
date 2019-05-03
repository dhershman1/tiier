module Board exposing (Board, empty)

import Color exposing (Color)
import Dict exposing (Dict)
import Random


type Terrain
    = Water
    | Wall
    | Floor
    | Forest
    | TownRoad
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


empty : Board
empty =
    Board
        { name = ""
        , id = ""
        , biome = ""
        , grid = Dict.empty
        , dungeon = False
        }


{-| The Board will be pulled from our DB to get its stats like Biome, name, dungeon, etc.
For now though we can also just fake that. Replace "fakeBoard" with an actual db return
-}
generate : Random.Seed -> String -> Board
generate seed id =
    empty
