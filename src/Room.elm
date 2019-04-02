module Room exposing (Room)

import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Task exposing (Task)
import Time


type alias Room =
    { seed : Random.Seed
    , grid : Grid
    , height : Random.Generator Int
    , width : Random.Generator Int
    }


getNow : Maybe Int
getNow =
    Iso8601.fromTime Time.now
        |> String.replace "-" ""
        |> String.replace "." ""
        |> String.replace ":" ""
        |> String.replace "Z" ""
        |> String.toInt


buildRoomList : Int -> Int -> Int -> List Room
buildRoomList maxHeight maxWidth maxCount =
    List.repeat maxCount
        { seed = Random.initialSeed (Iso8601.fromTime Time.now)
        , height = Random.int 5 maxHeight
        , width = Random.int 5 maxWidth
        }
