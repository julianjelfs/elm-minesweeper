module RandomPositions exposing (get)

import Random
import Set
import Task
import Time
import Types exposing (..)


rndPoint : Config -> Random.Generator Coord
rndPoint config =
    Random.pair (Random.int 0 (config.dimensions - 1)) (Random.int 0 (config.dimensions - 1))


rndSet : Config -> Random.Generator (Set.Set Coord)
rndSet config =
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
                    (rndPoint config)
    in
    addUniquePoints config.initialBombs Set.empty


rndPos : Config -> Random.Seed -> Set.Set Coord
rndPos config seed =
    Tuple.first <| Random.step (rndSet config) seed


posixToBombs : Config -> Time.Posix -> Task.Task never (Set.Set Coord)
posixToBombs config =
    Time.posixToMillis >> Random.initialSeed >> rndPos config >> Task.succeed


get : Config -> Cmd Msg
get config =
    Time.now
        |> Task.andThen (posixToBombs config)
        |> Task.perform Positions
