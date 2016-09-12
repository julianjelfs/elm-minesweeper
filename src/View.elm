module View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import Types exposing (..)
import String exposing (padLeft)

drawCell grid y x =
    let
        maybeCell = Dict.get (x, y) grid
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
                                            Just 0 -> ""
                                            Just n -> toString n)
                in
                    div
                        [ class cls
                        , onClick (ClickedCell cell) ]
                        [ text txt ]
            Nothing -> div [][]

drawRow grid y =
    div
        [ class "row" ]
        ([0..9] |> List.map (drawCell grid y))

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

durationToSeconds = (flip (/) 1000) >> round >> toString

youWin: Model -> Html Msg
youWin model =
    div
        [ class "you-win" ]
        [ div [] [ text ("Congratulations! You won in " ++ (model.duration |> durationToSeconds) ++ " seconds") ]
        , button
            [ class "restart"
            , onClick StartGame ]
            [ text "Start Again" ]
        ]

root: Model -> Html Msg
root model =
    div
        [ class "container" ]
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
        , div
            [ class "see-code" ]
            [ span []
                [ text "See the code "
                , a [ target "_blank", href "https://github.com/julianjelfs/elm-minesweeper"] [ text "here" ]
                ]
            ]
        , (case model.state of
            Won -> youWin model
            _ -> div [] [])
        ]
