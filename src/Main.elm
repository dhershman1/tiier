module Tiier exposing (State(..))

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Html exposing (..)
import Html.Events exposing (keyCode)



-- MODELS


type State
    = Paused
    | Playing
    | Stopped
