module Character exposing (Model)

import Gear
import Skills


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


type alias Model =
    { name : String
    , level : Int
    , exp : Float
    , skills : Skills.Model
    , stats : Stats
    , equipment : Gear.Model
    }
