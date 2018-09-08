module Types exposing (Cell, CellState(..), Coord, Model, Msg(..), State(..), addBombToGrid, addBombsToGrid, createCell, createGrid, dec, getCell, id, inc, initialModel, nearbyCells, populateNearbyBombs, translate)

import Config exposing (config)
import Debug exposing (..)
import Dict
import Set


type alias Cell =
    { x : Int
    , y : Int
    , state : CellState
    , bomb : Bool
    , nearbyBombs : Maybe Int
    }


type CellState
    = Hidden
    | Cleared
    | Flagged


type State
    = NewGame
    | Playing
    | Won
    | Lost


type alias Model =
    { grid : Dict.Dict Coord Cell
    , duration : Float
    , state : State
    , ctrl : Bool
    , numberOfBombs : Int
    , cellClicked : Maybe Coord
    }


type alias Coord =
    ( Int, Int )


type alias IsCtrl =
    Bool


type Msg
    = Positions (Set.Set Coord)
    | StartGame
    | Tick Float
    | ClickedCell Cell
    | KeyDown IsCtrl
    | KeyUp IsCtrl


createCell ( x, y ) =
    Cell x y Hidden False Nothing


createGrid =
    let
        row =
            \i -> List.range 0 9 |> List.map (\n -> ( i, n ))

        keys =
            List.concatMap row (List.range 0 9)
    in
    keys
        |> List.foldl (\t d -> Dict.insert t (createCell t) d) Dict.empty


initialModel : Model
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
    [ ( dec, dec )
    , ( dec, id )
    , ( dec, inc )
    , ( id, dec )
    , ( id, inc )
    , ( inc, dec )
    , ( inc, id )
    , ( inc, inc )
    ]
        |> List.filterMap (translate grid coord)


getCell =
    Dict.get


translate grid ( x, y ) ( dx, dy ) =
    Dict.get ( dx x, dy y ) grid


addBombToGrid coord grid =
    Dict.update coord
        (Maybe.map (\c -> { c | bomb = True }))
        grid


populateNearbyBombs grid =
    Dict.map
        (\k v ->
            let
                nearbyBombs =
                    nearbyCells grid k
                        |> List.filter .bomb
                        |> List.length
                        |> Just
            in
            { v | nearbyBombs = nearbyBombs }
        )
        grid


addBombsToGrid pos grid =
    Set.foldl addBombToGrid grid pos
        |> populateNearbyBombs
