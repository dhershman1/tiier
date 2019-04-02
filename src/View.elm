module View exposing (view)

import Grid exposing (Grid, produceGrid)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList, href, id)
import Messages exposing (Msg(..))
import Model exposing (Model)


view : Model -> Html Msg
view model =
    div []
        [ text "Testing 123"
        , div [ class "grid" ] [ text "The grid will live here" ]
        ]
