module Tiier exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Html exposing (..)
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Messages exposing (Msg(..))
import Model exposing (Model)
import Task
import Time
import Update
import View



-- MODELS


main : Program Value Model Msg
main =
    Browser.element
        { init =
            \value ->
                ( value
                    |> Decode.decodeValue Model.decode
                    |> Result.withDefault Model.initial
                , Cmd.batch
                    [ Task.perform GetViewport getViewport
                    , Task.perform InitRandom Time.now
                    ]
                )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize Resize
        , Sub.map KeyMsg Keyboard.subscriptions
        ]
