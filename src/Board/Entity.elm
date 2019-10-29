module Board.Entity exposing (Entity, EntityStats, fromString, toString)


type Entity
    = Character
    | Item
    | NPC
    | Decoration
    | Trap
    | Empty


type alias EntityStats =
    { name : String
    , entity : Entity
    }


toString : Entity -> String
toString entity =
    case entity of
        Character ->
            "character"

        Item ->
            "item"

        NPC ->
            "npc"

        Decoration ->
            "decoration"

        Trap ->
            "trap"

        Empty ->
            "empty"


fromString : String -> Entity
fromString str =
    case str of
        "character" ->
            Character

        "item" ->
            Item

        "npc" ->
            NPC

        "decoration" ->
            Decoration

        "trap" ->
            Trap

        _ ->
            Empty