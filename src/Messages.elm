module Messages exposing (Msg(..))

import Biome exposing (Biome)
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
    | Resize Int Int
    | KeyMsg Keyboard.Msg
    | SetBiome Biome
    | GetViewport Viewport
    | Noop
