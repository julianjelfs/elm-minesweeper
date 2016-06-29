module Main exposing(..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)
import Container
import One

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
    , container: Container.Model One.Model
    }

type Msg =
    NoOp
    | ContainerMsg (Container.Msg One.Msg)

createCell: Int -> Int -> Cell
createCell rowIndex cellIndex =
    Cell Hidden False

createRow: Int -> Row
createRow index =
    [0..9]
        |> List.map (createCell index)
        |> Row

createGrid: Grid
createGrid =
    [0..9]
        |> List.map createRow
        |> Grid

initialModel: Model
initialModel =
    Model createGrid 0 NewGame (Container.initialModel One.initialModel)

init : ( Model, Cmd Msg )
init =
  ( initialModel, Cmd.none )

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
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
        , Html.map ContainerMsg (Container.view model.container)
        ]

main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\m -> Sub.none)
        }
