module Biome exposing (Biome, fromString, getColorRanges, toString)

{-| Handles Biome information and conversions
-}

import Json.Decode as Decode
import Json.Encode as Encode


type alias ColorRanges =
    { wall : List (List Int)
    , ground : List (List Int)
    , foilage : List (List Int)
    }


type Biome
    = Forest
    | Frozen
    | Desert
    | Plains
    | Cave
    | None


toString : Biome -> String
toString b =
    case b of
        Forest ->
            "Forest"

        Frozen ->
            "Frozen"

        Desert ->
            "Desert"

        Plains ->
            "Plains"

        Cave ->
            "Cave"

        None ->
            ""


fromString : String -> Biome
fromString b =
    case b of
        "Forest" ->
            Forest

        "Frozen" ->
            Frozen

        "Desert" ->
            Desert

        "Plains" ->
            Plains

        "Cave" ->
            Cave

        _ ->
            None


getColorRanges : Biome -> ColorRanges
getColorRanges b =
    case b of
        Forest ->
            { wall = [ List.range 199 235, List.range 199 235, List.range 199 235 ]
            , ground = [ List.range 120 130, List.range 90 105, List.range 60 75 ]
            , foilage = [ List.range 30 45, List.range 190 205, List.range 60 75 ]
            }

        Frozen ->
            { wall = [ List.range 199 235, List.range 199 235, List.range 199 235 ]
            , ground = [ List.range 245 255, List.range 245 255, List.range 245 255 ]
            , foilage = [ List.range 180 190, List.range 200 215, List.range 245 255 ]
            }

        Desert ->
            { wall = [ List.range 241 250, List.range 225 250, List.range 175 250 ]
            , ground = [ List.range 250 255, List.range 240 245, List.range 190 210 ]
            , foilage = [ List.range 200 210, List.range 160 170, List.range 16 40 ]
            }

        Plains ->
            { wall = [ List.range 180 205, List.range 240 255, List.range 165 200 ]
            , ground = [ List.range 50 60, List.range 150 160, List.range 35 40 ]
            , foilage = [ List.range 80 100, List.range 220 240, List.range 35 45 ]
            }

        Cave ->
            { wall = [ List.range 199 235, List.range 199 235, List.range 199 235 ]
            , ground = [ List.range 210 240, List.range 195 210, List.range 235 240 ]
            , foilage = [ List.range 80 100, List.range 220 240, List.range 35 45 ]
            }

        None ->
            { wall = []
            , ground = []
            , foilage = []
            }
