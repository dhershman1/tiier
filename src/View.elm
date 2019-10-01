module View exposing (view)

import Color exposing (randomRgb)
import Debug exposing (log)
import Html exposing (Html, aside, button, div, footer, h1, header, main_, p, span, text)
import Html.Attributes exposing (attribute, class, classList, href, id, style, title)
import Html.Events exposing (onClick)
import Map.Board as Board
import Map.Tile as Tile
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

        "stairs" ->
            ( "rgba(236, 236, 0, 0.59)", Random.initialSeed 9 )

        "door" ->
            ( "rgb(248, 83, 97)", Random.initialSeed 10 )

        _ ->
            ( "rgb(0, 0, 0)", Random.initialSeed 12 )


renderCell : List Tile.Cell -> Int -> List (Html Msg) -> List (Html Msg)
renderCell cells n els =
    let
        rest =
            Maybe.withDefault [] (List.tail cells)

        current =
            Maybe.withDefault (Tile.floor ( 0, 0 )) (List.head cells)

        ( color, nextSeed ) =
            getColor (Tile.toString current.terrain) (Random.initialSeed n)
    in
    if List.length cells == 0 && List.isEmpty rest then
        els

    else
        renderCell rest
            (n + 1)
            (List.append els
                [ span [ class ("grid__cell grid__cell--" ++ Tile.toString current.terrain), style "background-color" color, title (Board.posToString current.pos) ] [ text current.char ]
                ]
            )


init : Model -> List (Html Msg)
init { board } =
    List.map (\c -> span [ class "grid__cell" ] [ text c.char ]) (Board.toList board)


view : Model -> Html Msg
view model =
    main_ [ class "tiier" ]
        [ header []
            [ h1 [] [ text "Tiier" ]
            , p [] [ text "Map Seed: ", text (String.fromInt model.initialInt) ]
            ]
        , aside [ class "details" ]
            [ span [ class "location" ] [ text "Location: " ]
            , span [] [ text model.board.name ]
            ]
        , div [ class "grid" ] (renderCell (Board.toList model.board) model.initialInt [])
        ]
