port module Update exposing (update)

import Board exposing (Board)
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
    let
        tmp =
            log "pressed keys" model.pressedKeys

        arrows =
            Keyboard.Arrows.wasd model.pressedKeys

        x =
            log "arrows x" arrows.x

        y =
            log "arrows y" arrows.y

        newPos =
            ( Tuple.first model.position - arrows.y
            , Tuple.second model.position + arrows.x
            )
    in
    { model
        | board = Board.moveCharacter model.board model.position newPos
        , position = newPos
    }


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
