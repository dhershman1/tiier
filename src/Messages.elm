module Messages exposing (Msg(..))

import Browser.Dom exposing (Viewport)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Random
import Time exposing (Posix)


type Msg
    = Start
    | Stop
    | Resume
    | Tick Float
    | InitRandom Posix
    | LoadBoard Posix String
    | Resize Int Int
    | KeyMsg Keyboard.Msg
    | GetViewport Viewport
    | Noop
