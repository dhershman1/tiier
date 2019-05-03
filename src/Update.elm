port module Update exposing (update)

import Biome exposing (Biome)
import Board exposing (Board)
import Cell exposing (Cell)
import Debug exposing (log)
import Dict exposing (Dict)
import Grid exposing (Grid)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Messages exposing (..)
import Model exposing (..)
import Random
import Time


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

        Tick _ ->
            ( model, Cmd.none )

        LoadBoard now id ->
            if Dict.member id model.loadedBoards then
                let
                    seed =
                        Dict.get id model.loadedBoards
                in
                ( { model
                    | loadedBoards = Dict.insert id (Maybe.withDefault (Random.initialSeed 0) seed) model.loadedBoards
                  }
                , Cmd.none
                )

            else
                let
                    seed =
                        Random.initialSeed (Time.posixToMillis now)
                in
                ( { model
                    | loadedBoards = Dict.insert id seed model.loadedBoards
                  }
                , Cmd.none
                )

        InitRandom now ->
            let
                seed =
                    Random.initialSeed <| log "initial seed" <| Time.posixToMillis now
            in
            ( { model
                | randomSeed = seed
              }
            , Cmd.none
            )

        SetBiome b ->
            ( { model | biome = b }, Cmd.none )

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
