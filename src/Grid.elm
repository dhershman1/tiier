module Grid exposing (Cell, Grid, cellToRecord, empty, encode, generateGrid, posFromString, posToString)

-- The Cell type will become what holds all the information of a cell as a user walks among them
-- For now though, I just want to get the app producing a grid
-- DamageTypes, Trap, and Event will all be moved out of here eventually

import Biome exposing (Biome)
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Random exposing (Generator, Seed)



-- type alias Grid =
--     List Cell


type Grid
    = Grid
        { cells : Dict String Cell
        , name : String
        , biome : Biome
        , roomCount : Int
        }


{-| Terrain types: (Largest they can be) This is in the manner of x and y axis
Wall: 1x1
Water: 6x7 (This is still up for debate)
Floor: 1x1 (Not Room)
Forest: 7x8 (Still up for debate)
Town Main Roads: 2x? (However long the road is)
Abyss: ?x? This is the fallback and the filler for unaccessible areas.
-}
type Terrain
    = Water
    | Wall
    | Floor
    | Forest
    | TownRoad
    | Abyss


type Neighbor
    = North
    | South
    | East
    | West


type alias CellRec =
    { char : String
    , passable : Bool
    , pos : ( Int, Int )
    }


type Cell
    = Cell CellRec


cellToRecord : Cell -> CellRec
cellToRecord (Cell { char, passable, pos }) =
    { char = char, passable = passable, pos = pos }


posToString : Cell -> String
posToString (Cell { pos }) =
    String.fromInt (Tuple.first pos) ++ ", " ++ String.fromInt (Tuple.second pos)


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


toList : Grid -> List ( String, Cell )
toList (Grid { cells }) =
    Dict.toList cells


empty : Grid
empty =
    Grid
        { name = ""
        , biome = Biome.fromString ""
        , cells = Dict.empty
        , roomCount = 0
        }


applyCells : Grid -> Dict String Cell -> Grid
applyCells (Grid { name, biome, roomCount }) c =
    Grid
        { name = name
        , biome = biome
        , roomCount = roomCount
        , cells = c
        }


{-| Get a weight based on the neighboring cells type
-}
getWeight : Grid -> ( Int, Int ) -> Float
getWeight (Grid { cells }) ( x, y ) =
    [ ( x - 2, y - 2 ), ( x - 1, y - 1 ), ( x + 1, y + 1 ), ( x + 2, y + 2 ) ]


{-| Generates a basic square grid and assigns it a seed to use
-}
generateGrid : Int -> Int -> Seed -> ( Grid, Seed )
generateGrid width height seed =
    let
        indicies =
            List.range 0 (height - 1)
                |> List.concatMap
                    (\y ->
                        List.range 0 (width - 1)
                            |> List.map
                                (\x ->
                                    ( String.fromInt x ++ ", " ++ String.fromInt y
                                    , Cell
                                        { char = "."
                                        , passable = True
                                        , pos = ( x, y )
                                        }
                                    )
                                )
                    )
    in
    ( applyCells empty
        (Dict.fromList indicies)
    , seed
    )



-- generateRooms : Int -> Int -> Seed -> Grid -> Grid
-- generateRooms min max seed grid =


decodeCell : Decode.Decoder Cell
decodeCell =
    Decode.map5
        (\char passable posX posY ->
            Cell
                { char = char
                , passable = passable
                , pos = ( posX, posY )
                }
        )
        (Decode.field "char" Decode.string)
        (Decode.field "passable" Decode.bool)
        (Decode.field "posX" Decode.int)
        (Decode.field "posY" Decode.int)


encodeCell : Cell -> Encode.Value
encodeCell (Cell { char, pos }) =
    Encode.object
        [ ( "char", Encode.string char )
        , ( "posX", Encode.int (Tuple.first pos) )
        , ( "posY", Encode.int (Tuple.second pos) )
        ]


decode : Decode.Decoder Grid
decode =
    Decode.map3
        (\name biome roomCount ->
            Grid
                { name = name
                , biome = biome
                , cells = cells
                , roomCount = roomCount
                }
        )
        (Decode.field "name" Decode.string)
        (Decode.field "biome" (Decode.map Biome.fromString Decode.string))
        (Decode.field "roomCount" Decode.int)


encode : Int -> Grid -> String
encode indent (Grid { name, biome, roomCount }) =
    Encode.encode
        indent
        (Encode.object
            [ ( "name", Encode.string name )
            , ( "biome", Encode.string (Biome.toString biome) )
            , ( "roomCount", Encode.int roomCount )
            ]
        )
