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

ctrl = 17
initialBombs = 15

type alias Grid =
    { rows: List Row
    }

type alias Row =
    { rowIndex: Int
    , cells: List Cell
    }

type alias Cell =
    { rowIndex: Int
    , cellIndex: Int
    , state: CellState
    }

type CellState = Hidden
    | Cleared
    | Flagged

type State = NewGame
    | Playing
    | Won
    | Lost

type alias Model = 
    { grid: Grid
    , duration: Time.Time
    , state: State
    , ctrl: Bool
    , bombs: Set.Set Coord
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

createCell: Int -> Int -> Cell
createCell rowIndex cellIndex =
    Cell rowIndex cellIndex Hidden

dec x = x - 1

createRow: Int -> Row
createRow index =
    [0..(dimensions |> fst |> dec)]
        |> List.map (createCell index)
        |> Row index

createGrid: Grid
createGrid =
    [0..(dimensions |> snd |> dec)]
        |> List.map createRow
        |> Grid

initialModel: Model
initialModel =
    Model createGrid 0 NewGame False Set.empty initialBombs

init : ( Model, Cmd Msg )
init =
  ( initialModel, Cmd.none )

--the problem with this is that we are using a set so duplicates will be ignored so really we need to
--generating numbers until we get n, rather than just looping n times
randomPositions n =
    Time.now `Task.andThen`
    (\t ->
        let
            s = Random.initialSeed (round t)
        in
            [0..n]
                |> List.foldl
                    (\i (s, st) ->
                        let
                            (r, s1) = Random.step (Random.int 0 9) s
                            (c, s2) = Random.step (Random.int 0 9) s1
                        in
                            (s2, (Set.insert (r,c) st)))
                    (s, Set.empty)
                |> snd
                |> Task.succeed)
            |> Task.perform Dummy Positions

--UPDATE
cellsMatch c1 c2 =
    c1.rowIndex == c2.rowIndex
        && c1.cellIndex == c2.cellIndex

replaceCell cell grid =
    { grid | rows =
        (grid.rows
            |> List.map
                (\r ->
                    { r | cells =
                        (r.cells
                            |> List.map
                                (\c ->
                                    if (cellsMatch c cell) then
                                        cell
                                    else
                                        c ) ) } ) ) }

findNearbyBombs bombs cell =
    0

revealCell model cell =
    let
        bombs = model.bombs
        cellContainsBomb = Set.member (cell.rowIndex, cell.cellIndex) bombs
    in
        case cellContainsBomb of
            True ->
                { model | state = Lost }
            False ->
                let
                    nearbyBombs = findNearbyBombs bombs cell
                in
                    case nearbyBombs of
                        0 ->
                            -- if there are 0, show that cell and reveal all surrounding cells
                            model
                        _ ->
                            -- if there are > 0, show that cell
                            model

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
                ({ model | bombs = pos }, Cmd.none)

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
drawCell: Cell -> Html Msg
drawCell cell =
    let
        cls =
            case cell.state of
                Hidden -> "cell hidden"
                Flagged -> "cell flagged"
                Cleared -> "cell cleared"
    in
        div
            [ class cls
            , onClick (ClickedCell cell) ]
            [ ]

drawRow: Row -> Html Msg
drawRow row =
    div
        [ class "row" ]
        (row.cells |> List.map drawCell)

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

padLeftNum n =
    n |> toString |> padLeft 3 '0'

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
                (model.grid.rows |> List.map drawRow)
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
