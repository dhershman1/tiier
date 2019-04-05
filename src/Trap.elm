module Trap exposing (Trap)

import Map exposing (Biome)


type DamageTypes
    = Physical
    | Fire
    | Frost
    | Earth
    | Electric
    | Poison
    | Celestial
    | Chaos


type Trap
    = Trap
        { dmgType : DamageTypes
        , dmgAmt : Int
        , avoidable : Bool
        , baseAvoidPercent : Int
        , name : String
        }
