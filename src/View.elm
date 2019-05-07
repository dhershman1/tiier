module View exposing (view)

import Board exposing (boardToList, generate)
import Html exposing (Html, aside, button, div, footer, h1, header, main_, span, text)
import Html.Attributes exposing (attribute, class, classList, href, id)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Model exposing (Model)
import Task
import Time


init : Model -> List (Html Msg)
init { board } =
    List.map (\c -> span [ class "grid__cell" ] [ text c.char ]) (boardToList board)


view : Model -> Html (Cmd Msg)
view model =
    main_ [ class "tiier" ]
        [ header [] [ h1 [] [ text "Tiier" ] ]
        , aside [ class "details" ]
            [ span [ class "location" ] [ text "Location: " ]
            , span [] [ text "Nowhere" ]
            , button [ onClick (Time.now |> Task.perform TestLoad) ] [ text "Generate" ]
            ]
        , div [ class "grid" ] (List.map (\c -> span [ class "grid__cell" ] [ text c.char ]) (boardToList model.board))
        ]
