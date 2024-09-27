module RandomPositions exposing (get)

import Config exposing (config)
import Random
import Set
import Task
import Time
import Types exposing (..)


rndPoint : Random.Generator Coord
rndPoint =
    Random.pair (Random.int 0 (config.dimensions - 1)) (Random.int 0 (config.dimensions - 1))


rndSet : Int -> Random.Generator (Set.Set Coord)
rndSet n =
    let
        addUniquePoints : Int -> Set.Set Coord -> Random.Generator (Set.Set Coord)
        addUniquePoints remaining set =
            if remaining == 0 then
                Random.constant set

            else
                Random.andThen
                    (\p ->
                        if Set.member p set then
                            addUniquePoints remaining set

                        else
                            addUniquePoints (remaining - 1) (Set.insert p set)
                    )
                    rndPoint
    in
    addUniquePoints n Set.empty


rndPos : Int -> Random.Seed -> Set.Set Coord
rndPos n seed =
    Tuple.first <| Random.step (rndSet n) seed


posixToBombs : Time.Posix -> Task.Task never (Set.Set Coord)
posixToBombs =
    Time.posixToMillis >> Random.initialSeed >> rndPos config.initialBombs >> Task.succeed


get : Cmd Msg
get =
    Time.now
        |> Task.andThen posixToBombs
        |> Task.perform Positions
