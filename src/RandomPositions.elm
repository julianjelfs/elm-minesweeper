module RandomPositions exposing (get)

import Set
import Random
import Task
import Time
import Types exposing (..)
import Config exposing (config)

rndPos : Int -> Set.Set Coord -> Random.Seed -> Set.Set Coord
rndPos n pos seed =
    let
        num = Set.size pos
    in
        if num == n then
            pos
        else
            let
                (r, seed2) = Random.step (Random.int 0 9) seed
                (c, seed3) = Random.step (Random.int 0 9) seed2
            in
                rndPos n (Set.insert (r, c) pos) seed3


get =
    Time.now `Task.andThen`
        (\t ->
            round t
                |> Random.initialSeed
                |> (rndPos config.initialBombs Set.empty)
                |> Task.succeed )
                |> Task.perform Dummy Positions
