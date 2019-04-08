module View exposing (view)

import Grid exposing (Cell, Grid, cellToRecord, encode, getCells, initialize)
import Html exposing (Html, button, div, header, main_, text)
import Html.Attributes exposing (attribute, class, classList, href, id)
import Messages exposing (Msg(..))
import Model exposing (Model)


cellToAttrs : Cell -> List (Html.Attribute msg)
cellToAttrs c =
    let
        cec =
            cellToRecord c
    in
    [ attribute "x" (String.fromInt (Tuple.first cec.pos)), attribute "y" (String.fromInt (Tuple.second cec.pos)) ]


getPos : Cell -> String
getPos c =
    let
        cec =
            cellToRecord c
    in
    "X: " ++ String.fromInt (Tuple.first cec.pos) ++ " Y: " ++ String.fromInt (Tuple.second cec.pos)


viewCells : List Cell -> List (Html msg)
viewCells cells =
    List.map (\c -> div (List.concat [ [ class "grid__cell" ], cellToAttrs c ]) [ text (getPos c) ]) cells


view : Model -> Html Msg
view model =
    main_ [ class "tiier" ]
        [ header [] []
        , div [ class "grid" ] (viewCells (getCells (initialize 20 20)))
        ]
