module View exposing (root)

import Button
import Dict
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import String exposing (padLeft)
import Types exposing (..)


drawCell : Config -> Dict.Dict Coord Cell -> Int -> Int -> Html Msg
drawCell config grid y x =
    case Dict.get ( x, y ) grid of
        Just cell ->
            let
                ( cls, txt ) =
                    case cell.state of
                        Hidden ->
                            ( "cell hidden", "" )

                        Flagged ->
                            ( "cell flagged", "" )

                        Cleared ->
                            if cell.bomb then
                                ( "cell cleared bomb", "" )

                            else
                                ( "cell cleared"
                                , case cell.nearbyBombs of
                                    Nothing ->
                                        ""

                                    Just 0 ->
                                        ""

                                    Just n ->
                                        String.fromInt n
                                )
            in
            div
                [ class cls
                , onClick (ClickedCell cell)
                , style "height" (String.fromFloat config.cellSize ++ "px")
                , style "width" (String.fromFloat config.cellSize ++ "px")
                ]
                [ text txt ]

        Nothing ->
            div [] []


drawRow : Config -> Dict.Dict Coord Cell -> Int -> Html Msg
drawRow config grid y =
    div [ class "row" ]
        (List.range 0 (config.dimensions.columns - 1) |> List.map (drawCell config grid y))


levelName : Level -> String
levelName level =
    case level of
        Easy ->
            "Easy"

        Normal ->
            "Normal"

        Hard ->
            "Hard"

        Hardcore ->
            "Hardcore"


levelToggle : Model -> Html Msg
levelToggle model =
    Button.button (levelName model.level) ToggleLevel


startButton : Model -> Html Msg
startButton model =
    let
        cls =
            case model.state of
                Lost ->
                    "start-button sad"

                _ ->
                    "start-button happy"
    in
    button
        [ class cls
        , onClick StartGame
        ]
        []


padLeftNum : Int -> String
padLeftNum =
    String.fromInt >> padLeft 3 '0'


bombCount : Model -> Html Msg
bombCount model =
    div [ class "bomb-count" ]
        [ div [ class "bomb-icon" ] [], text (padLeftNum model.numberOfBombs) ]


timer : Model -> Html Msg
timer model =
    div [ class "timer" ]
        [ div [ class "timer-icon" ] []
        , text (durationToSeconds model.duration)
        ]


header : Model -> Html Msg
header model =
    div [ class "header" ]
        [ div [ class "left" ]
            [ startButton model
            , levelToggle model
            ]
        , div [ class "right" ]
            [ bombCount model
            , timer model
            ]
        ]


durationToSeconds : Float -> String
durationToSeconds =
    (\a -> a / 1000) >> round >> padLeftNum


youWin : Model -> Html Msg
youWin model =
    div [ class "modal" ]
        [ div [ class "game-over" ]
            [ div [] [ text ("Congratulations! You won in " ++ (model.duration |> durationToSeconds) ++ " seconds") ]
            , Button.button "Start Again" StartGame
            ]
        ]


youLose : Html Msg
youLose =
    div [ class "modal" ]
        [ div [ class "game-over" ]
            [ div [] [ text "Sorry you lost - come on it's not that hard" ]
            , Button.button "Try Again" StartGame
            ]
        ]


root : Model -> Html Msg
root model =
    div [ class "container" ]
        [ div
            [ class "game-area"
            , classList
                [ ( "easy", model.level == Easy )
                , ( "normal", model.level == Normal )
                , ( "hard", model.level == Hard )
                , ( "hardcore", model.level == Hardcore )
                ]
            ]
            [ header model
            , div [ class "grid", HA.id "game-grid" ]
                (List.range 0 (model.config.dimensions.rows - 1) |> List.map (drawRow model.config model.grid))
            , div [ class "footer" ]
                [ p [] [ text <| "Hello " ++ model.username ]
                , p [] [ text "Click to reveal a square, Ctrl or Cmd click to flag a square" ]
                ]
            ]
        , case model.state of
            Won ->
                youWin model

            Lost ->
                youLose

            _ ->
                div [] []
        ]
