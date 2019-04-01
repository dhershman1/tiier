module Messages exposing (Msg(..))

import Browser.Dom exposing (Viewport)
import Random


type Msg
    = Start
    | Pause
    | Resume
    | LoadRoom Random.Seed
    | LoadMap Random.Seed
    | MoveLeft Bool
    | MoveRight Bool
    | MoveDown Bool
    | MoveUp Bool
    | OpenMenu Bool
    | GetViewport Viewport
    | Noop
