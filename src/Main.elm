module Tiier exposing (State(..))

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Html exposing (..)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Messages exposing (Msg(..))
import Model exposing (Model)
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
                , Task.perform GetViewport getViewport
                )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.state == Model.Playing then
            onAnimationFrameDelta Tick

          else
            Sub.none
        , onKeyUp (Decode.map (key False) keyCode)
        , onKeyDown (Decode.map (key True) keyCode)
        , onResize Resize
        ]


key : Bool -> Int -> Msg
key on keycode =
    case keycode of
        37 ->
            MoveLeft on

        39 ->
            MoveRight on

        40 ->
            MoveDown on

        38 ->
            MoveUp on

        27 ->
            OpenMenu on

        _ ->
            Noop
