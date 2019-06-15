module View exposing (view)

import Html exposing (Html, aside, button, div, footer, h1, header, main_, span, text)
import Html.Attributes exposing (attribute, class, classList, href, id, title)
import Html.Events exposing (onClick)
import Map.Board exposing (boardToList, generate, posToString, terrainToClass)
import Messages exposing (Msg(..))
import Model exposing (Model)
import Task
import Time


init : Model -> List (Html Msg)
init { board } =
    List.map (\c -> span [ class "grid__cell" ] [ text c.char ]) (boardToList board)


view : Model -> Html Msg
view model =
    main_ [ class "tiier" ]
        [ header [] [ h1 [] [ text "Tiier" ] ]
        , aside [ class "details" ]
            [ span [ class "location" ] [ text "Location: " ]
            , span [] [ text "Nowhere" ]
            ]
        , div [ class "grid" ] (List.map (\c -> span [ class (String.append "grid__cell " (terrainToClass c)), title (posToString c.pos) ] [ text c.char ]) (boardToList model.board))
        ]
