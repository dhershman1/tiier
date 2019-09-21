module Map.Terrain exposing (Terrain(..), empty, fromString, toString)


type Terrain
    = Water
    | Wall
    | Floor
    | Forest
    | Abyss


fromString : String -> Terrain
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

        _ ->
            Abyss


toString : Terrain -> String
toString terrain =
    case terrain of
        Wall ->
            "wall"

        Water ->
            "water"

        Floor ->
            "floor"

        Forest ->
            "forest"

        Abyss ->
            "abyss"


empty : Terrain
empty =
    Abyss
