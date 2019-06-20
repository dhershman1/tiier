module AI.Pathfinding exposing (Path, Position, findPath, pythagoreanCost, straightLineCost)

{-| The pathfinding AI is mostly built for building out paths on the map the user can use
This approach means we can formulate a start, and exit and build multiple paths to those locations
We are using a rendition of the A\* (astar) path finding algorithm
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Encode as Encode
import Set exposing (Set)
import Tuple exposing (first, second)


type alias Model comparable =
    { evaluated : Set comparable
    , openSet : Set comparable
    , costs : Dict comparable Float
    , cameFrom : Dict comparable comparable
    }


{-| A position is just a pair of (x,y) coordinates.
-}
type alias Position =
    ( Int, Int )


{-| A path is a `List` of `Position`s.
-}
type alias Path =
    List Position


initialModel : comparable -> Model comparable
initialModel start =
    { evaluated = Set.empty
    , openSet = Set.singleton start
    , costs = Dict.singleton start 0
    , cameFrom = Dict.empty
    }


planPath :
    (comparable -> comparable -> Float)
    -> (comparable -> Set comparable)
    -> comparable
    -> comparable
    -> Maybe (List comparable)
planPath costFn moveFn start end =
    initialModel start
        |> astar costFn moveFn end
        |> Maybe.map Array.toList


cheapestOpen : (comparable -> Float) -> Model comparable -> Maybe comparable
cheapestOpen costFn model =
    model.openSet
        |> Set.toList
        |> List.filterMap
            (\position ->
                case Dict.get position model.costs of
                    Nothing ->
                        Nothing

                    Just cost ->
                        Just ( position, cost + costFn position )
            )
        |> List.sortBy second
        |> List.head
        |> Maybe.map first


reconstructPath : Dict comparable comparable -> comparable -> Array comparable
reconstructPath cameFrom goal =
    case Dict.get goal cameFrom of
        Nothing ->
            Array.empty

        Just next ->
            Array.push goal
                (reconstructPath cameFrom next)


updateCost : comparable -> comparable -> Model comparable -> Model comparable
updateCost current neighbour model =
    let
        newCameFrom =
            Dict.insert neighbour current model.cameFrom

        newCosts =
            Dict.insert neighbour distanceTo model.costs

        distanceTo =
            reconstructPath newCameFrom neighbour
                |> Array.length
                |> toFloat

        newModel =
            { model
                | costs = newCosts
                , cameFrom = newCameFrom
            }
    in
    case Dict.get neighbour model.costs of
        Nothing ->
            newModel

        Just previousDistance ->
            if distanceTo < previousDistance then
                newModel

            else
                model


astar :
    (comparable -> comparable -> Float)
    -> (comparable -> Set comparable)
    -> comparable
    -> Model comparable
    -> Maybe (Array comparable)
astar costFn moveFn goal model =
    case cheapestOpen (costFn goal) model of
        Nothing ->
            Nothing

        Just current ->
            if current == goal then
                Just (reconstructPath model.cameFrom goal)

            else
                let
                    modelPopped =
                        { model
                            | openSet = Set.remove current model.openSet
                            , evaluated = Set.insert current model.evaluated
                        }

                    neighbours =
                        moveFn current

                    newNeighbours =
                        Set.diff neighbours modelPopped.evaluated

                    modelWithNeighbours =
                        { modelPopped
                            | openSet =
                                Set.union modelPopped.openSet
                                    newNeighbours
                        }

                    modelWithCosts =
                        Set.foldl (updateCost current) modelWithNeighbours newNeighbours
                in
                astar costFn moveFn goal modelWithCosts


findPath :
    (Position -> Position -> Float)
    -> (Position -> Set Position)
    -> Position
    -> Position
    -> Maybe Path
findPath =
    planPath


{-| A simple costing algorithm. Think of it as the number of moves a
rook/castle would have to make on a chessboard.
-}
straightLineCost : Position -> Position -> Float
straightLineCost ( x1, y1 ) ( x2, y2 ) =
    let
        dx =
            abs (x1 - x2)

        dy =
            abs (y1 - y2)
    in
    toFloat <| dx + dy


{-| An alternative costing algorithm, which calculates pythagorean distance.
-}
pythagoreanCost : Position -> Position -> Float
pythagoreanCost ( x1, y1 ) ( x2, y2 ) =
    let
        dx =
            toFloat <| abs (x1 - x2)

        dy =
            toFloat <| abs (y1 - y2)
    in
    abs <| (sqrt 2 * min dx dy) + abs (dy - dx)
