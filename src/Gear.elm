module Gear exposing (Accessory, Equipment, Model)

-- This piece should handle all the buffs and boosts given with gear
-- Needs a magical property bonus when we decide to add that


type alias Equipment =
    { name : String
    , armor : Int
    , rarity : String
    }



-- Needs the magical/boost bonus when it's decided on how it'll work


type alias Accessory =
    { name : String
    , rarity : String
    }


type alias Model =
    { head : Equipment
    , chest : Equipment
    , hands : Equipment
    , feet : Equipment
    , neck : Accessory
    , leftFinger : Accessory
    , rightFinger : Accessory
    }
