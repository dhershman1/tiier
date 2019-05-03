module View exposing (view)

import Cell exposing (Cell)
import Html exposing (Html, aside, button, div, footer, h1, header, main_, span, text)
import Html.Attributes exposing (attribute, class, classList, href, id)
import Messages exposing (Msg(..))
import Model exposing (Model)


view : Model -> Html Msg
view model =
    main_ [ class "tiier" ]
        [ header [] [ h1 [] [ text "Tiier" ] ]
        , aside [ class "details" ] [ span [ class "location" ] [ text "Location: " ], span [] [ text "Nowhere" ] ]
        , div [ class "grid" ] [ text "Grid!" ])
        ]
