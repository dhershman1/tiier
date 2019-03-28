module Messages exposing (Msg(..))

import Browser.Dom exposing (Viewport)


type Msg
    = Start
    | Pause
    | Resume
    | MoveLeft Bool
    | MoveRight Bool
    | MoveDown Bool
    | MoveUp Bool
    | GetViewport Viewport
    | Noop
