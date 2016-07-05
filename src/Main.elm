module Main exposing(..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)
import Time
import Task
import Random

type alias Grid =
    { rows: List Row
    }

type alias Row =
    { cells: List Cell
    }

type alias Cell =
    { state: CellState
    , bomb: Bool
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
    , time: Int
    , state: State
    }

type alias Coord = (Int, Int)

type Msg =
    Dummy ()
        | Positions (List Coord)
        | NoOp

dimensions = 
    (10, 10)

createCell: Int -> Int -> Cell
createCell rowIndex cellIndex =
    Cell Hidden False

dec x = x - 1

createRow: Int -> Row
createRow index =
    [0..(dimensions |> fst |> dec)]
        |> List.map (createCell index)
        |> Row

createGrid: Grid
createGrid =
    [0..(dimensions |> snd |> dec)]
        |> List.map createRow
        |> Grid


initialModel: Model
initialModel =
    Model createGrid 0 NewGame

init : ( Model, Cmd Msg )
init =
  ( initialModel, randomPositions )


positions n t =
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
            |> Task.succeed

randomPositions =
    let
        pos = Time.now `Task.andThen` (positions 10)
    in
        Task.perform Dummy Positions pos

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Dummy () ->
            (model, Cmd.none)
        Positions pos ->
            let
                x = log "Pos: " pos
            in
                (model, Cmd.none)
        NoOp ->
            (model, Cmd.none)


drawCell: Cell -> Html Msg
drawCell cell =
    div
        [ class "cell" ]
        [ text "Cell" ]

drawRow: Row -> Html Msg
drawRow row =
    div
        [ class "row" ]
        (row.cells |> List.map drawCell)

view: Model -> Html Msg
view model =
    div
        []
        [ div
            [ class "instructions"]
            [ text "It's Minesweeper - you probably know what to do ..." ]
        , div
            [ class "game-area" ]
            [ div
                [ class "header" ]
                [ text "we'll put the stats and the clock in here" ]
            , div
                [ class "grid" ]
                (model.grid.rows |> List.map drawRow)
            ]
        ]

main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\m -> Sub.none)
        }
