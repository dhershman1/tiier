port module Update exposing (update)

import Messages exposing (..)
import Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model
                | state = Playing
              }
            , Cmd.none
            )

        Pause ->
            { model | state = Paused }

        Resume ->
            ( { model | state = Playing }
            , Cmd.none
            )

        Noop ->
            ( model, Cmd.none )
