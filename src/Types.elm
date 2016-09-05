module Types exposing (..)

import Set
import Keyboard
import Time
import Dict
import Config exposing (config)

type alias Cell =
    { rowIndex: Int
    , cellIndex: Int
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

createCell (rowIndex, cellIndex) =
    Cell rowIndex cellIndex Hidden False Nothing

createGrid =
    let
        row = (\i -> [0..9] |> List.map (\n -> (i, n)))
        keys = List.concatMap row [0..9]
    in
        keys
            |> List.foldl (\t d -> Dict.insert t (createCell t) d) Dict.empty

initialModel: Model
initialModel =
    Model createGrid 0 NewGame False config.initialBombs

addBombToGrid coord grid =
    Dict.update coord (\mc ->
        case mc of
            Just c -> Just { c | bomb = True }
            Nothing -> Nothing) grid

addBombsToGrid pos grid =
    Set.foldl addBombToGrid grid pos
