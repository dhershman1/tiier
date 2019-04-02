module Messages exposing (Msg(..))

import Browser.Dom exposing (Viewport)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Random


type Msg
    = Start
    | Stop
    | Resume
    | Resize Int Int
    | KeyMsg Keyboard.Msg
    | LoadRoom Random.Seed
    | LoadMap Random.Seed
    | GetViewport Viewport
    | Noop
