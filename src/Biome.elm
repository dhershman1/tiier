module Biome exposing (Biome, ColorRanges, fromString, getColorRanges, toString)

{-| Handles Biome information and conversions
-}

import Json.Decode as Decode
import Json.Encode as Encode


type alias ColorRanges =
    { wall : List ( Int, Int )
    , ground : List ( Int, Int )
    , foilage : List ( Int, Int )
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
            { wall = [ ( 199, 235 ), ( 199, 235 ), ( 199, 235 ) ]
            , ground = [ ( 120, 130 ), ( 90, 105 ), ( 60, 75 ) ]
            , foilage = [ ( 30, 45 ), ( 190, 205 ), ( 60, 75 ) ]
            }

        Frozen ->
            { wall = [ ( 199, 235 ), ( 199, 235 ), ( 199, 235 ) ]
            , ground = [ ( 245, 255 ), ( 245, 255 ), ( 245, 255 ) ]
            , foilage = [ ( 180, 190 ), ( 200, 215 ), ( 245, 255 ) ]
            }

        Desert ->
            { wall = [ ( 241, 250 ), ( 225, 250 ), ( 175, 250 ) ]
            , ground = [ ( 250, 255 ), ( 240, 245 ), ( 190, 210 ) ]
            , foilage = [ ( 200, 210 ), ( 160, 170 ), ( 16, 40 ) ]
            }

        Plains ->
            { wall = [ ( 180, 205 ), ( 240, 255 ), ( 165, 200 ) ]
            , ground = [ ( 50, 60 ), ( 150, 160 ), ( 35, 40 ) ]
            , foilage = [ ( 80, 100 ), ( 220, 240 ), ( 35, 45 ) ]
            }

        Cave ->
            { wall = [ ( 199, 235 ), ( 199, 235 ), ( 199, 235 ) ]
            , ground = [ ( 210, 240 ), ( 195, 210 ), ( 235, 240 ) ]
            , foilage = [ ( 80, 100 ), ( 220, 240 ), ( 35, 45 ) ]
            }

        None ->
            { wall = []
            , ground = []
            , foilage = []
            }
