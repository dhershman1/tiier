module View exposing (view)

import Color exposing (randomRgb)
import Html exposing (Html, aside, button, div, footer, h1, header, main_, span, text)
import Html.Attributes exposing (attribute, class, classList, href, id, style, title)
import Html.Events exposing (onClick)
import Map.Board exposing (Cell, boardToList, generate, posToString, strToTerrain, terrainToClass)
import Messages exposing (Msg(..))
import Model exposing (Model)
import Random
import Task
import Time


getColor : String -> Random.Seed -> ( String, Random.Seed )
getColor terrain seed =
    case terrain of
        "wall" ->
            randomRgb ( 233, 236 ) ( 219, 233 ) ( 221, 233 ) seed

        "floor" ->
            randomRgb ( 177, 196 ) ( 108, 130 ) ( 62, 87 ) seed

        "water" ->
            randomRgb ( 87, 121 ) ( 87, 121 ) ( 255, 255 ) seed

        _ ->
            ( "rgb(255, 255, 255)", Random.initialSeed 12 )


renderCell : List Cell -> Random.Seed -> List (Html Msg) -> List (Html Msg)
renderCell cells seed els =
    let
        rest =
            Maybe.withDefault [] (List.tail cells)

        current =
            Maybe.withDefault (Cell "." True (strToTerrain "floor") ( 0, 0 )) (List.head cells)

        ( color, nextSeed ) =
            getColor (terrainToClass current) seed
    in
    if List.length cells == 0 && List.isEmpty rest then
        els

    else
        renderCell rest nextSeed (List.append els [ span [ class ("grid__cell " ++ terrainToClass current), style "background-color" color, title (posToString current.pos) ] [ text current.char ] ])


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
        , div [ class "grid" ] (renderCell (boardToList model.board) model.randomSeed [])
        ]
