module View exposing (view)

import Html exposing (Html, button, div, text)
import Messages exposing (Msg(..))
import Model exposing (Model)


view : Model -> Html Msg
view model =
    div [] [ text "Testing 123" ]
