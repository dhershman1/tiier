module Character exposing (Character)

import Gear exposing (Gear)
import Skills exposing (Skills)


type alias ElResist =
    { fire : Int
    , frost : Int
    , earth : Int
    , electric : Int
    }


type alias Stats =
    { health : Int
    , damage : Int
    , dmgMultiplier : Float
    , criticalChance : Int
    , criticalMulti : Int
    , armor : Int
    , cooldownReduce : Int
    , hitChance : Float
    , elementalResist : ElResist
    }


type alias Character =
    { name : String
    , level : Int
    , exp : Float
    , skills : Skills
    , stats : Stats
    , equipment : Gear
    }
