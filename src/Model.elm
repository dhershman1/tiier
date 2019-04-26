module Model exposing (Model, State(..), decode, decodeState, encode, encodeState, initial)

-- import Grid exposing (Grid)

import Biome exposing (Biome)
import Json.Decode as Decode
import Json.Encode as Encode
import Keyboard exposing (Key(..))
import Map exposing (Map)
import Messages exposing (Msg)
import Random exposing (Generator, Seed)


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
    { state : State
    , pressedKeys : List Key
    , size : ( Float, Float )
    , difficulty : Difficulty
    , position : ( Int, Int )
    , randomSeed : Seed
    , map : Map
    , biome : Biome
    }


initial : Model
initial =
    { size = ( 0, 0 )
    , pressedKeys = []
    , state = Stopped
    , difficulty = Easy
    , position = ( 0, 0 )
    , randomSeed = Random.initialSeed 0
    , map = Map.empty
    , biome = Biome.fromString "Forest"
    }


decode : Decode.Decoder Model
decode =
    Decode.map5
        (\posX posY state difficulty biome ->
            { initial
                | position = ( posX, posY )
                , state = state
                , difficulty = difficulty
                , biome = biome
            }
        )
        (Decode.field "posX" Decode.int)
        (Decode.field "posY" Decode.int)
        (Decode.field "state" (Decode.map decodeState Decode.string))
        (Decode.field "difficulty" (Decode.map decodeDifficulty Decode.string))
        (Decode.field "biome" (Decode.map Biome.fromString Decode.string))


encode : Int -> Model -> String
encode indent model =
    Encode.encode
        indent
        (Encode.object
            [ ( "posX", Encode.int (Tuple.first model.position) )
            , ( "posY", Encode.int (Tuple.second model.position) )
            , ( "state", Encode.string (encodeState model.state) )
            , ( "difficulty", Encode.string (encodeDifficulty model.difficulty) )
            , ( "biome", Encode.string (Biome.toString model.biome) )
            ]
        )
