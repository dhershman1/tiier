module Room exposing (Room)

import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Task
import Time


type alias Room =
    { seed : Random.Seed
    , height : Random.Generator Int
    , width : Random.Generator Int
    }


buildRoomList : Int -> Int -> Int -> List Room
buildRoomList maxHeight maxWidth maxCount =
    List.repeat maxCount
        { seed = Random.initialSeed 0
        , height = Random.int 5 maxHeight
        , width = Random.int 5 maxWidth
        }
