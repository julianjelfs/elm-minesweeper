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
    , numberOfBombs: Int
    }

type alias Coord = (Int, Int)

type Msg =
    Dummy ()
    | Positions (List Coord)
    | StartGame Int
    | Tick Time.Time
    | ClickedCell Cell

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
    Model createGrid 0 NewGame 15

init : ( Model, Cmd Msg )
init =
  ( initialModel, Cmd.none )

randomPositions n =
    Time.now `Task.andThen`
    (\t ->
        let
            s = Random.initialSeed (round t)
        in
            [0..n]
                |> List.foldl
                    (\i (s, l) ->
                        let
                            (r, s1) = Random.step (Random.int 0 9) s
                            (c, s2) = Random.step (Random.int 0 9) s1
                        in
                            (s2, (r,c)::l))
                    (s, [])
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

flagCell grid cell =
    grid |> (replaceCell { cell | state = Flagged })

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        startGame = (\m n ->
            ( { m | state = Playing, grid = createGrid }, (randomPositions n) ))
    in
        case msg of
            Dummy () ->
                (model, Cmd.none)

            Positions pos ->
                (model, Cmd.none)

            StartGame n ->
                startGame model n

            Tick t ->
                ( { model | duration = model.duration + t }, Cmd.none )

            ClickedCell cell ->
                case model.state of
                    Playing ->
                        ( { model | grid = (flagCell model.grid cell) }, Cmd.none )
                    _ ->
                        let
                            (m, fx) =
                                startGame model model.numberOfBombs
                        in
                            ( { m | grid = (flagCell m.grid cell) }, fx )


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
        , onClick (StartGame model.numberOfBombs) ]
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

--WIRING

main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = timerSub
        }
