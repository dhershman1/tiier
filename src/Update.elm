port module Update exposing (update)

import Biome exposing (Biome)
import Cell exposing (Cell)
import Grid exposing (Grid)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Messages exposing (..)
import Model exposing (..)


port save : String -> Cmd msg


saveToStorage : Model -> ( Model, Cmd Msg )
saveToStorage model =
    ( model, save (Model.encode 2 model) )



-- TODO: Working on getting a biome to load over the map and grid
-- animate : Float -> Model -> Model
-- animate elapsed model =
--     model
--         |> moveTetrimino elapsed
--         |> rotateTetrimino elapsed
--         |> dropTetrimino elapsed
--         |> checkEndGame


moving : Model -> Model
moving model =
    let
        arrows =
            Keyboard.Arrows.wasd model.pressedKeys
    in
    { model
        | position =
            ( Tuple.first model.position + arrows.x
            , Tuple.second model.position + arrows.y
            )
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize width height ->
            ( { model | size = ( toFloat width, toFloat height ) }
            , Cmd.none
            )

        GetViewport { viewport } ->
            ( { model
                | size =
                    ( viewport.width
                    , viewport.height
                    )
              }
            , Cmd.none
            )

        Start ->
            ( { model
                | state = Playing
              }
            , Cmd.none
            )

        Stop ->
            saveToStorage { model | state = Paused }

        Resume ->
            ( { model | state = Playing }
            , Cmd.none
            )

        LoadRoom seed ->
            ( { model | currentRoom = "test" }
            , Cmd.none
            )

        LoadMap seed ->
            ( { model | currentMap = "test" }
            , Cmd.none
            )

        LoadBiome b ->
            let
                colorPallette =
                    Biome.getColorRanges b

                indicies =
                    List.range 0 (28 - 1)
                        |> List.concatMap
                            (\y ->
                                List.range 0 (50 - 1)
                                    |> List.map (\x -> ( x, y ))
                            )
            in
            let
                data =
                    indicies
                        |> List.map (\( x, y ) -> Cell.create { char = ".", passable = True, pos = ( x, y ) })
            in
            Grid.create 50 28 cells

        KeyMsg key ->
            ( { model | pressedKeys = Keyboard.update key model.pressedKeys }
            , Cmd.none
            )

        -- Tick time ->
        --     model
        --         |> animate (min time 25)
        --         |> saveToStorage
        Noop ->
            ( model, Cmd.none )
