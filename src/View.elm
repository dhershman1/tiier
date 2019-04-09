module View exposing (view)

import Cell exposing (Cell)
import Grid exposing (Grid, encode, initialize, toList)
import Html exposing (Html, aside, button, div, footer, h1, header, main_, span, text)
import Html.Attributes exposing (attribute, class, classList, href, id)
import Messages exposing (Msg(..))
import Model exposing (Model)


cellToAttrs : Cell -> List (Html.Attribute msg)
cellToAttrs c =
    let
        cec =
            Cell.toRecord c
    in
    [ attribute "x" (String.fromInt (Tuple.first cec.pos)), attribute "y" (String.fromInt (Tuple.second cec.pos)) ]


viewCells : List Cell -> List (Html msg)
viewCells cells =
    List.map (\c -> span (List.concat [ [ class "grid__cell" ], cellToAttrs c ]) [ text (Cell.toRecord c).char ]) cells


view : Model -> Html Msg
view model =
    main_ [ class "tiier" ]
        [ header [] [ h1 [] [ text "Tiier" ] ]
        , aside [ class "details" ] [ span [ class "location" ] [ text "Location: " ], span [] [ text "Nowhere" ] ]
        , div [ class "grid" ] (viewCells (toList (initialize 50 28)))
        ]
