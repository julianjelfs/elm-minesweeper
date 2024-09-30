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
    case model of
        Initialising _ ->
            div [] []

        Initialised gs ->
            Button.button (levelName gs.level) ToggleLevel


startButton : Model -> Html Msg
startButton model =
    let
        lost =
            case model of
                Initialised gs ->
                    gs.state == Lost

                _ ->
                    False
    in
    button
        [ class "start-button"
        , classList [ ( "sad", lost ), ( "happy", not lost ) ]
        , onClick StartGame
        ]
        []


padLeftNum : Int -> String
padLeftNum =
    String.fromInt >> padLeft 3 '0'


bombCount : Model -> Html Msg
bombCount model =
    div [ class "bomb-count" ]
        [ div [ class "bomb-icon" ] [], text (padLeftNum (propFromModel model .numberOfBombs 0)) ]


durationFromModel : Model -> Float
durationFromModel model =
    propFromModel model .duration 0


timer : Model -> Html Msg
timer model =
    div [ class "timer" ]
        [ div [ class "timer-icon" ] []
        , text (durationToSeconds <| durationFromModel model)
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
        [ div [ class "modal-content" ]
            [ div [] [ text ("Congratulations! You won in " ++ (durationToSeconds <| durationFromModel model) ++ " seconds") ]
            , Button.button "Start Again" StartGame
            ]
        ]


youLose : Html Msg
youLose =
    div [ class "modal" ]
        [ div [ class "modal-content" ]
            [ div [] [ text "Sorry you lost - come on it's not that hard" ]
            , Button.button "Try Again" StartGame
            ]
        ]


instructions : String -> Html Msg
instructions user =
    div [ class "modal" ]
        [ div [ class "modal-content" ]
            [ div []
                [ p [] [ text <| "Hello @" ++ user ]
                , p [] [ text "Click to reveal a square, Ctrl or Cmd click to flag a square" ]
                ]
            , Button.button "Got It" (ShowInstructions False)
            ]
        ]


levelFromModel : Model -> Level
levelFromModel model =
    propFromModel model .level Normal


root : Model -> Html Msg
root model =
    let
        level =
            levelFromModel model

        instr =
            propFromModel model .instructions False

        user =
            case model of
                Initialised { username } ->
                    username

                Initialising { username } ->
                    username
    in
    div [ class "container" ]
        [ div
            [ class "game-area"
            , classList
                [ ( "easy", level == Easy )
                , ( "normal", level == Normal )
                , ( "hard", level == Hard )
                , ( "hardcore", level == Hardcore )
                ]
            ]
            [ header model
            , div [ class "grid", HA.id "game-grid" ]
                (case model of
                    Initialising _ ->
                        []

                    Initialised gameState ->
                        List.range 0 (gameState.config.dimensions.rows - 1) |> List.map (drawRow gameState.config gameState.grid)
                )
            , if instr then
                instructions user

              else
                text ""
            ]
        , case model of
            Initialising _ ->
                div [] []

            Initialised gameState ->
                case gameState.state of
                    Won ->
                        youWin model

                    Lost ->
                        youLose

                    _ ->
                        div [] []
        ]
