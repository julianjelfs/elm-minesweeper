module Types exposing (..)

import Set
import Keyboard
import Time
import Dict
import Config exposing (config)
import Debug exposing (..)

type alias Cell =
    { x: Int
    , y: Int
    , state: CellState
    , bomb: Bool
    , nearbyBombs: Maybe Int
    }

type CellState = Hidden
    | Cleared
    | Flagged

type State = NewGame
    | Playing
    | Won
    | Lost

type alias Model =
    { grid: Dict.Dict Coord Cell
    , duration: Time.Time
    , state: State
    , ctrl: Bool
    , numberOfBombs: Int
    , cellClicked : Maybe Coord
    }

type alias Coord = (Int, Int)

type Msg =
    Dummy ()
    | Positions (Set.Set Coord)
    | StartGame
    | Tick Time.Time
    | ClickedCell Cell
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode

createCell (x, y) =
    Cell x y Hidden False Nothing

createGrid =
    let
        row = (\i -> [0..9] |> List.map (\n -> (i, n)))
        keys = List.concatMap row [0..9]
    in
        keys
            |> List.foldl (\t d -> Dict.insert t (createCell t) d) Dict.empty

initialModel: Model
initialModel =
    Model createGrid 0 NewGame False config.initialBombs Nothing

inc x =
    x + 1

dec x =
    x - 1

id x =
    x

{--
(-1,-1) (0, -1) (1, -1)
(-1, 0) (0,0)   (1, 0)
(-1, 1) (0, 1)  (1, 1)
--}
nearbyCells grid coord =
    [ (dec, dec)
    , (dec, id)
    , (dec, inc)
    , (id, dec)
    , (id, inc)
    , (inc, dec)
    , (inc, id)
    , (inc, inc) ]
        |> List.filterMap (translate grid coord)

getCell grid coord =
    Dict.get coord grid

translate grid (x, y) (dx, dy) =
    Dict.get (dx x, dy y) grid

addBombToGrid coord grid =
    Dict.update coord (\mc ->
        case mc of
            Just c -> Just { c | bomb = True}
            Nothing -> Nothing) grid

populateNearbyBombs grid =
    Dict.map (\k v ->
        let
            nearbyBombs =
                nearbyCells grid k
                    |> List.filter .bomb
                    |> List.length
                    |> Just
        in
            { v | nearbyBombs = nearbyBombs }) grid

addBombsToGrid pos grid =
    Set.foldl addBombToGrid grid pos
        |> populateNearbyBombs
