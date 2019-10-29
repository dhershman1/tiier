module Board.Entity exposing (Entity, EntityDetails, empty, fromString, toString)


type Entity
    = Character
    | Item
    | NPC
    | Decoration
    | Trap
    | Empty


type alias EntityDetails =
    { name : String
    , entity : Entity
    , char : String
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


empty : EntityDetails
empty =
    { name = "", entity = Empty, char = "" }
