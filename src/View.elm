module View exposing (view)

import Grid exposing (Cell, Grid, produceGrid)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList, href, id)
import Messages exposing (Msg(..))
import Model exposing (Model)


access : Cell -> String
access cell =
    "x: " ++ String.fromInt (Tuple.first cell) ++ " y: " ++ String.fromInt (Tuple.second cell)


pieceTogether : Grid -> String
pieceTogether li =
    List.foldl (++) "" (List.map access li)


view : Model -> Html Msg
view model =
    div []
        [ text "Testing 123"
        , div [ class "grid" ] [ text (pieceTogether (produceGrid 20)) ]
        ]
