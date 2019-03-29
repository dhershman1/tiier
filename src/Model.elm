module Model exposing (Model, State(..))

import Character exposing (Character)
import Map.Grid exposing (Grid)


type State
    = Paused
    | Playing
    | Stopped



-- Make sure we add the Grid and all other extras to our model


type alias Model =
    { moveLeft : Bool
    , moveRight : Bool
    , moveDown : Bool
    , moveUp : Bool
    , state : State
    , char : Character
    , position : ( Int, Float )
    , grid : Grid
    }
