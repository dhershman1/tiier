module Map.Tile exposing (Cell, Tile(..), abyss, door, empty, floor, fromString, toString, wall, water)


type alias Point =
    ( Int, Int )


type alias Cell =
    { char : String
    , cost : Float
    , passable : Bool
    , terrain : Tile
    , pos : Point
    }


type Tile
    = Water
    | Wall
    | Floor
    | Forest
    | Door
    | IronDoor
    | Abyss


door : Point -> Cell
door pos =
    Cell "+" 2 True Door pos


wall : Point -> Cell
wall pos =
    Cell "#" (1 / 0) False Wall pos


floor : Point -> Cell
floor pos =
    Cell "." 1 True Floor pos


water : Point -> Cell
water pos =
    Cell "~" 1 True Water pos


abyss : Point -> Cell
abyss pos =
    Cell "" (1 / 0) False Abyss pos


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
