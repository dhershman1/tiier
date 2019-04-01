module Model exposing (Model, State(..), decodeState, encodeState, initial)

import Grid exposing (Grid)
import Json.Decode as Decode
import Json.Encode as Encode
import Random


type State
    = Paused
    | Playing
    | Stopped


type Difficulty
    = Easy
    | Normal
    | Hard


decodeDifficulty : String -> Difficulty
decodeDifficulty string =
    case string of
        "easy" ->
            Easy

        "normal" ->
            Normal

        _ ->
            Hard


encodeDifficulty : Difficulty -> String
encodeDifficulty diff =
    case diff of
        Easy ->
            "easy"

        Normal ->
            "normal"

        Hard ->
            "hard"


decodeState : String -> State
decodeState string =
    case string of
        "paused" ->
            Paused

        "playing" ->
            Playing

        _ ->
            Stopped


encodeState : State -> String
encodeState state =
    case state of
        Paused ->
            "paused"

        Playing ->
            "playing"

        Stopped ->
            "stopped"



-- Make sure we add the Grid and all other extras to our model


type alias Model =
    { moveLeft : Bool
    , moveRight : Bool
    , moveDown : Bool
    , moveUp : Bool
    , state : State
    , difficulty : Difficulty
    , position : ( Int, Float )
    , currentMap : Random.Seed
    , currentRoom : Random.Seed
    }


initial : Model
initial =
    { moveLeft = False
    , moveRight = False
    , moveDown = False
    , moveUp = False
    , state = Stopped
    , difficulty = Easy
    , position = ( 0, 0 )
    , currentMap = ""
    , currentRoom = ""
    }


decode : Decode.Decoder Model
decode =
    Decode.map6
        (\posX posY state difficulty mapSeed roomSeed ->
            { initial
                | position = ( posX, posY )
                , state = state
                , difficulty = difficulty
                , currentMap = mapSeed
                , currentRoom = roomSeed
            }
        )
        (Decode.field "posX" Decode.int)
        (Decode.field "posY" Decode.float)
        (Decode.field "state" (Decode.map decodeState Decode.string))
        (Decode.field "difficulty" (Decode.map decodeDifficulty Decode.string))
        (Decode.field "currentMap" Decode.string)
        (Decode.field "currentRoom" Decode.string)


encode : Int -> Model -> String
encode indent model =
    Encode.encode
        indent
        (Encode.object
            [ ( "posX", Encode.int (Tuple.first model.position) )
            , ( "posY", Encode.float (Tuple.second model.position) )
            , ( "state", Encode.string (encodeState model.state) )
            , ( "difficulty", Encode.string (encodeDifficulty model.difficulty) )
            , ( "currentMap", Encode.string model.currentMap )
            , ( "currentRoom", Encode.string model.currentRoom )
            ]
        )
