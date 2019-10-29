port module Update exposing (update)

import Board exposing (Board)
import Board.Entity as Entity
import Board.Tile as Tile
import Debug exposing (log)
import Dict exposing (Dict)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Messages exposing (..)
import Model exposing (..)
import Random
import Task
import Time


port save : String -> Cmd msg


saveToStorage : Model -> ( Model, Cmd Msg )
saveToStorage model =
    ( model, save (Model.encode 2 model) )


moving : Model -> Model
moving model =
    if List.isEmpty model.pressedKeys then
        model

    else
        let
            tmp =
                log "pressed keys" <| model.pressedKeys

            { x, y } =
                log "arrows" <| Keyboard.Arrows.wasd model.pressedKeys

            ( currX, currY ) =
                model.position

            newPos =
                -- Up
                if x == 0 && y == 1 then
                    log "Up" ( currX - y, currY - x )
                    -- Right

                else if x == 1 && y == 0 then
                    log "Right " ( currX + y, currY + x )
                    -- Left

                else if x == -1 && y == 0 then
                    log "Left" ( currX + y, currY - x )
                    -- Down

                else
                    log "Down" ( currX - y, currY + x )

            currPos =
                log "currPos" model.position

            tile =
                Maybe.withDefault (Tile.abyss newPos) (Dict.get newPos model.board.grid)

            player =
                log "player" <| Maybe.withDefault Entity.empty (Dict.get currPos model.board.actors)
        in
        if tile.passable then
            { model
                | board = Board.moveCharacter model.board currPos newPos player
                , position = newPos
            }

        else
            model


getNewTime : Cmd Msg
getNewTime =
    Task.perform InitRandom Time.now


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

        InitRandom now ->
            let
                time =
                    Time.posixToMillis now

                seed =
                    Random.initialSeed <| log "initial seed" <| time

                ( board, pos ) =
                    Board.generate 50 95 seed
            in
            ( { model
                | randomSeed = seed
                , initialInt = time
                , board = board
                , position = pos
              }
            , Cmd.none
            )

        KeyMsg key ->
            let
                newModel =
                    { model | pressedKeys = Keyboard.update key model.pressedKeys }
            in
            ( moving newModel, Cmd.none )

        -- Tick time ->
        --     model
        --         |> animate (min time 25)
        --         |> saveToStorage
        Noop ->
            ( model, Cmd.none )
