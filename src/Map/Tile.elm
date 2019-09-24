module Map.Tile exposing (Tile(..), empty, fromString, toString)


type Tile
    = Water
    | Wall
    | Floor
    | Forest
    | Door
    | IronDoor
    | Abyss


fromString : String -> Tile
fromString str =
    case str of
        "wall" ->
            Wall

        "water" ->
            Water

        "floor" ->
            Floor

        "forest" ->
            Forest

        "door" ->
            Door

        "iron-door" ->
            IronDoor

        _ ->
            Abyss


toString : Tile -> String
toString tile =
    case tile of
        Wall ->
            "wall"

        Water ->
            "water"

        Floor ->
            "floor"

        Forest ->
            "forest"

        Door ->
            "door"

        IronDoor ->
            "iron-door"

        Abyss ->
            "abyss"


empty : Tile
empty =
    Abyss
