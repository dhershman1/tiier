module Model exposing (Model, State(..), initial)

import Grid exposing (Grid)


type State
    = Paused
    | Playing
    | Stopped


type Difficulty
    = Easy
    | Normal
    | Hard
    | Hardcore



-- Make sure we add the Grid and all other extras to our model


type alias Model =
    { moveLeft : Bool
    , moveRight : Bool
    , moveDown : Bool
    , moveUp : Bool
    , state : State
    , difficulty : Difficulty
    , position : ( Int, Float )
    }


initial : Model
initial =
    { moveLeft = False
    , moveRight = False
    , moveDown = False
    , moveUp = False
    , state = Stopped
    , difficulty = Easy
    , position = ( 0, 0 )
    }
