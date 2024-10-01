module Types exposing
    ( Cell
    , CellState(..)
    , Config
    , Coord
    , Dimensions
    , Flags
    , GameState
    , Grid
    , Level(..)
    , Model(..)
    , Msg(..)
    , State(..)
    , addBombToGrid
    , addBombsToGrid
    , createCell
    , createGrid
    , dec
    , getCell
    , getConfig
    , id
    , inc
    , initialModel
    , levelToInt
    , nearbyCells
    , populateNearbyBombs
    , propFromModel
    , translate
    )

import Browser.Dom exposing (Error)
import Debug exposing (..)
import Dict
import Set


type alias Config =
    { dimensions : { rows : Int, columns : Int }, initialBombs : Int, cellSize : Float }


type alias Flags =
    { username : String
    , level : Level
    , instructions : Bool
    }



-- this is not right because the height needs to be just the height of the game-area not the whole window
-- which means that we need to measure it in Elm land *before* we have rendered the grid


type alias Dimensions =
    { width : Float, height : Float }


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


type Level
    = Easy
    | Normal
    | Hard
    | Hardcore


levelToInt : Level -> Int
levelToInt level =
    case level of
        Easy ->
            0

        Normal ->
            1

        Hard ->
            2

        Hardcore ->
            3


type Model
    = Initialising Flags
    | Initialised GameState


type alias GameState =
    { grid : Grid
    , duration : Float
    , state : State
    , ctrl : Bool
    , numberOfBombs : Int
    , cellClicked : Maybe Coord
    , username : String
    , config : Config
    , level : Level
    , dimensions : Dimensions
    , instructions : Bool
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
    | ToggleLevel
    | GetDimensions
    | GotDimensions (Result Error Dimensions)
    | ShowInstructions Bool
    | Resize


createCell : ( Int, Int ) -> Cell
createCell ( x, y ) =
    Cell x y Hidden False Nothing


createGrid : Config -> Grid
createGrid config =
    let
        row =
            \c -> List.range 0 (config.dimensions.rows - 1) |> List.map (\r -> ( c, r ))

        keys =
            List.concatMap row (List.range 0 (config.dimensions.columns - 1))
    in
    keys
        |> List.foldl (\t d -> Dict.insert t (createCell t) d) Dict.empty


propFromModel : Model -> (GameState -> a) -> a -> a
propFromModel model fn def =
    case model of
        Initialising _ ->
            def

        Initialised gs ->
            fn gs


initialModel : Flags -> Maybe Dimensions -> Model
initialModel flags dimensions =
    case dimensions of
        Nothing ->
            Initialising flags

        Just dims ->
            let
                config =
                    getConfig dims flags.level
            in
            Initialised
                { grid = createGrid config
                , duration = 0
                , state = NewGame
                , ctrl = False
                , numberOfBombs = config.initialBombs
                , cellClicked = Nothing
                , username = flags.username
                , config = config
                , level = flags.level
                , dimensions = dims
                , instructions = flags.instructions
                }


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


getConfig : Dimensions -> Level -> Config
getConfig dimensions level =
    let
        numRows : Float -> Int
        numRows size =
            floor <| dimensions.height / size
    in
    case level of
        Easy ->
            let
                size =
                    dimensions.width / 12
            in
            { dimensions = { rows = numRows size, columns = 12 }
            , initialBombs = 10
            , cellSize = size
            }

        Normal ->
            let
                size =
                    dimensions.width / 18
            in
            { dimensions = { rows = numRows size, columns = 18 }
            , initialBombs = 40
            , cellSize = size
            }

        Hard ->
            let
                size =
                    dimensions.width / 30
            in
            { dimensions = { rows = numRows size, columns = 30 }
            , initialBombs = 200
            , cellSize = size
            }

        Hardcore ->
            let
                size =
                    dimensions.width / 50
            in
            { dimensions = { rows = numRows size, columns = 50 }
            , initialBombs = 400
            , cellSize = size
            }
