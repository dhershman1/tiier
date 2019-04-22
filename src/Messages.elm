module Messages exposing (Msg(..))

import Biome exposing (Biome)
import Browser.Dom exposing (Viewport)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Random


type Msg
    = Start
    | Stop
    | Resume
      -- | Tick Float
    | Resize Int Int
    | KeyMsg Keyboard.Msg
    | LoadRoom Random.Seed
    | LoadMap Random.Seed
    | SetBiome Biome
    | GetViewport Viewport
    | Noop
