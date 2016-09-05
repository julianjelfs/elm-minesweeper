module View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import Types exposing (..)
import String exposing (padLeft)

drawCell grid rowIndex cellIndex =
    let
        maybeCell = Dict.get (rowIndex, cellIndex) grid
    in
        case maybeCell of
            Just cell ->
                let
                    (cls, txt) =
                         case cell.state of
                            Hidden -> ("cell hidden", "")
                            Flagged -> ("cell flagged", "")
                            Cleared ->
                                case cell.bomb of
                                    True -> ("cell cleared bomb", "")
                                    False -> ("cell cleared",
                                        case cell.nearbyBombs of
                                            Nothing -> ""
                                            Just n -> toString n)
                in
                    div
                        [ class cls
                        , onClick (ClickedCell cell) ]
                        --[ text ((toString cell.rowIndex) ++ "," ++ (toString cell.cellIndex)) ]
                        [ text txt ]
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

root: Model -> Html Msg
root model =
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
