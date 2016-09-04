module Main exposing(..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)
import Time
import Task
import Random
import AnimationFrame exposing (diffs)
import String exposing (padLeft)
import Keyboard
import Set
import Dict

ctrl = 17
initialBombs = 15

type alias Cell =
    { rowIndex: Int
    , cellIndex: Int
    , state: CellState
    , bomb: Bool
    , nearbyBombs: Int
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

dimensions = 
    (10, 10)

createCell (rowIndex, cellIndex) =
    Cell rowIndex cellIndex Hidden False 0

dec x = x - 1

createGrid =
    let
        row = (\i -> [0..9] |> List.map (\n -> (i, n)))
        keys = List.concatMap row [0..9]
    in
        keys
            |> List.foldl (\t d -> Dict.insert t (createCell t) d) Dict.empty

initialModel: Model
initialModel =
    Model createGrid 0 NewGame False initialBombs

init : ( Model, Cmd Msg )
init =
  ( initialModel, Cmd.none )


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


randomPositions n =
    Time.now `Task.andThen`
        (\t ->
            round t
                |> Random.initialSeed
                |> (rndPos n Set.empty)
                |> Task.succeed )
                |> Task.perform Dummy Positions

--UPDATE
cellsMatch c1 c2 =
    c1.rowIndex == c2.rowIndex
        && c1.cellIndex == c2.cellIndex

replaceCell : Cell -> Dict.Dict Coord Cell -> Dict.Dict Coord Cell
replaceCell cell grid =
    let
        k = (cell.rowIndex, cell.cellIndex)
    in
        Dict.update k (\mc ->
            case mc of
                Just c -> Just cell
                Nothing -> mc)  grid

nearbyCells grid cell =
    []

findNearbyBombs model cell =
    (nearbyCells model.grid cell)
        |> List.filter .bomb
        |> List.length

revealCell model cell =
    case cell.bomb of
        True ->
            { model | state = Lost
            , grid = model.grid
                |> (replaceCell { cell | state = Cleared }) }
        False ->
            let
                nearbyBombs = findNearbyBombs model cell
                updatedModel =
                    { model | grid = model.grid
                            |> (replaceCell { cell | state = Cleared }) }
            in
                case nearbyBombs of
                    0 ->
                        -- if there are 0, show that cell and reveal all surrounding cells
                        updatedModel
                    _ ->
                        -- if there are > 0, show that cell
                        updatedModel

flagCell model cell =
    let
        changeState = (\b s ->
            { model | numberOfBombs = b
            , grid = model.grid |> (replaceCell { cell | state = s }) })
    in
        case cell.state of
            Flagged ->
                changeState (model.numberOfBombs + 1) Hidden
            Hidden ->
                changeState (model.numberOfBombs - 1) Flagged
            _ ->
                model

addBombToGrid coord grid =
    Dict.update coord (\mc ->
        case mc of
            Just c -> Just { c | bomb = True }
            Nothing -> Nothing) grid

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        startGame = (\m ->
            ( { initialModel | state = Playing, ctrl = m.ctrl }, (randomPositions initialBombs) ))

        ctrlClick = (\m k b ->
            case k of
                ctrl ->
                    ( { m | ctrl = b }, Cmd.none ))

        handleClick = (\m c ->
            case m.ctrl of
                True ->
                    (flagCell m c)
                False ->
                    (revealCell m c) )
    in
        case msg of
            Dummy () ->
                (model, Cmd.none)

            Positions pos ->
                let
                    grid =
                        pos
                            |> Set.foldl addBombToGrid model.grid
                in
                    ({ model | grid = grid }, Cmd.none)

            StartGame ->
                startGame model

            Tick t ->
                ( { model | duration = model.duration + t }, Cmd.none )

            ClickedCell cell ->
                case model.state of
                    Playing ->
                        ( (handleClick model cell), Cmd.none )
                    _ ->
                        let
                            (m, fx) =
                                startGame model
                        in
                            ( (handleClick m cell), fx )

            KeyDown k ->
                ctrlClick model k True

            KeyUp k ->
                ctrlClick model k False


--VIEW STUFF
drawCell grid rowIndex cellIndex =
    let
        maybeCell = Dict.get (rowIndex, cellIndex) grid
    in
        case maybeCell of
            Just cell ->
                let
                    cls =
                        case cell.state of
                            Hidden -> "cell hidden"
                            Flagged -> "cell flagged"
                            Cleared ->
                                case cell.bomb of
                                    True -> "cell cleared bomb"
                                    False -> "cell cleared"
                in
                    div
                        [ class cls
                        , onClick (ClickedCell cell) ]
                        [ ]
            Nothing -> div [][]

drawRow grid rowIndex =
    div
        [ class "row" ]
        ([0..9] |> List.map (drawCell grid rowIndex))

startButton model =
    let
        cls =
            case model.state of
                Lost -> "start-button sad"
                _ -> "start-button happy"
    in
    button
        [ class cls
        , onClick StartGame ]
        []

padLeftNum =
    toString >> (padLeft 3 '0')

bombCount model =
    span
        [ class "bomb-count" ]
        [ text (padLeftNum model.numberOfBombs) ]

timer model =
    let
        elapsed = model.duration / 1000 |> round
    in
        span
            [ class "timer" ]
            [ text (padLeftNum elapsed) ]

header: Model -> Html Msg
header model =
    div
        [ class "header" ]
        [ bombCount model
        , startButton model
        , timer model
        ]

view: Model -> Html Msg
view model =
    div
        []
        [ div
            [ class "instructions"]
            [ text "Click to reveal a square, Ctrl-click to flag a square" ]
        , div
            [ class "game-area" ]
            [ header model
            , div
                [ class "grid" ]
                ([0..9] |> List.map (drawRow model.grid))
            ]
        ]

timerSub model =
    case model.state of
        Playing ->
            diffs Tick
        _ -> Sub.none

keyBoard =
    Sub.batch [Keyboard.downs KeyDown, Keyboard.ups KeyUp]

--WIRING

main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\m -> Sub.batch [timerSub m, keyBoard])
        }
