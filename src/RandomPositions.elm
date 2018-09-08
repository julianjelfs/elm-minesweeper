module RandomPositions exposing (get)

import Config exposing (config)
import Random
import Set
import Task
import Time
import Types exposing (..)


rndPoint : Random.Generator Coord
rndPoint =
    Random.pair (Random.int 0 9) (Random.int 0 9)


rndSet : Int -> Random.Generator (Set.Set Coord)
rndSet n =
    Random.map Set.fromList <| Random.list n rndPoint


rndPos : Int -> Random.Seed -> Set.Set Coord
rndPos n seed =
    Tuple.first <| Random.step (rndSet n) seed


posixToBombs =
    Time.posixToMillis >> Random.initialSeed >> rndPos config.initialBombs >> Task.succeed


get =
    Time.now
        |> Task.andThen posixToBombs
        |> Task.perform Positions
