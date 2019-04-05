module View exposing (view)

import Grid exposing (Cell, Grid, encode, initialize)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList, href, id)
import Messages exposing (Msg(..))
import Model exposing (Model)


view : Model -> Html Msg
view model =
    div []
        [ text "Testing 123"
        , div [ class "grid" ] [ text (encode 2 (initialize 19 19)) ]
        ]
