module Map.Terrain exposing (Terrain(..), empty, strToTerrain, terrainToStr)


type Terrain
    = Water
    | Wall
    | Floor
    | Forest
    | Abyss


strToTerrain : String -> Terrain
strToTerrain str =
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


terrainToStr : Terrain -> String
terrainToStr terrain =
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
