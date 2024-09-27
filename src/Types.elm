module Types exposing (Cell, CellState(..), Coord, Flags, Grid, Model, Msg(..), State(..), addBombToGrid, addBombsToGrid, createCell, createGrid, dec, getCell, id, inc, initialModel, nearbyCells, populateNearbyBombs, translate)

import Config exposing (config)
import Debug exposing (..)
import Dict
import Set


type alias Flags =
    { username : String
    }


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


type alias Grid =
    Dict.Dict Coord Cell


type alias CoordModifier =
    Int -> Int


type alias Model =
    { grid : Grid
    , duration : Float
    , state : State
    , ctrl : Bool
    , numberOfBombs : Int
    , cellClicked : Maybe Coord
    , username : String
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


createCell : ( Int, Int ) -> Cell
createCell ( x, y ) =
    Cell x y Hidden False Nothing


createGrid : Grid
createGrid =
    let
        row =
            \i -> List.range 0 (config.dimensions - 1) |> List.map (\n -> ( i, n ))

        keys =
            List.concatMap row (List.range 0 (config.dimensions - 1))
    in
    keys
        |> List.foldl (\t d -> Dict.insert t (createCell t) d) Dict.empty


initialModel : Flags -> Model
initialModel flags =
    Model createGrid 0 NewGame False config.initialBombs Nothing flags.username


inc : Int -> Int
inc x =
    x + 1


dec : Int -> Int
dec x =
    x - 1


id : x -> x
id x =
    x



{--
(-1,-1) (0, -1) (1, -1)
(-1, 0) (0,0)   (1, 0)
(-1, 1) (0, 1)  (1, 1)
--}


nearbyCells : Grid -> Coord -> List Cell
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


getCell : Coord -> Grid -> Maybe Cell
getCell =
    Dict.get


translate : Grid -> Coord -> ( CoordModifier, CoordModifier ) -> Maybe Cell
translate grid ( x, y ) ( dx, dy ) =
    Dict.get ( dx x, dy y ) grid


addBombToGrid : Coord -> Grid -> Grid
addBombToGrid coord =
    Dict.update coord
        (Maybe.map (\c -> { c | bomb = True }))


populateNearbyBombs : Grid -> Grid
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


addBombsToGrid : Set.Set Coord -> Grid -> Grid
addBombsToGrid pos grid =
    Set.foldl addBombToGrid grid pos
        |> populateNearbyBombs
