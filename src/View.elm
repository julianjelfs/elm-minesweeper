module View exposing (root)

import Browser.Events exposing (onMouseMove)
import Button
import Dict
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Types exposing (..)


bombsToString : Maybe Int -> String
bombsToString bombs =
    case bombs of
        Nothing ->
            ""

        Just 0 ->
            ""

        Just n ->
            String.fromInt n


onTouchStart : msg -> Html.Attribute msg
onTouchStart msg =
    Html.Events.on "touchstart" (JD.succeed msg)


onTouchEnd : msg -> Html.Attribute msg
onTouchEnd msg =
    Html.Events.on "touchend" (JD.succeed msg)


appendIf : Bool -> List a -> List a -> List a
appendIf pred more list =
    if pred then
        List.append list more

    else
        list


drawCell : Level -> Bool -> Config -> Dict.Dict Coord Cell -> Int -> Int -> Html Msg
drawCell level touch config grid y x =
    case Dict.get ( x, y ) grid of
        Just cell ->
            let
                ( cls, txt ) =
                    case cell.state of
                        Hidden ->
                            ( "cell hidden"
                            , ""
                            )

                        Flagged ->
                            ( "cell flagged", "" )

                        Cleared ->
                            if cell.bomb then
                                ( "cell cleared bomb", "" )

                            else
                                ( "cell cleared"
                                , bombsToString cell.nearbyBombs
                                )
            in
            if touch then
                div
                    ([ class cls
                     , classList [ ( "touch", touch ) ]
                     , onTouchStart (StartPress cell)
                     , onTouchEnd EndPress
                     , Html.Events.on "touchmove" (JD.succeed MouseMove)
                     , style "height" (String.fromFloat config.cellSize ++ "px")
                     , style "width" (String.fromFloat config.cellSize ++ "px")
                     ]
                        |> appendIf (level /= Easy)
                            [ style "min-width" "40px"
                            , style "min-height" "40px"
                            ]
                    )
                    [ text txt ]

            else
                div
                    [ class cls
                    , classList [ ( "touch", touch ) ]
                    , onClick (ClickedCell cell)
                    , style "height" (String.fromFloat config.cellSize ++ "px")
                    , style "width" (String.fromFloat config.cellSize ++ "px")
                    ]
                    [ text txt ]

        Nothing ->
            div [] []


drawRow : Level -> Bool -> Config -> Dict.Dict Coord Cell -> Int -> Html Msg
drawRow level touch config grid y =
    div [ class "row" ]
        (List.range 0 (config.dimensions.columns - 1) |> List.map (drawCell level touch config grid y))


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
    Button.button (levelName model.flags.level) ToggleLevel


startButton : Model -> Html Msg
startButton model =
    button
        [ class "start-button"
        , classList [ ( "sad", model.state == Lost ), ( "happy", not (model.state == Lost) ) ]
        , onClick StartGame
        ]
        []


highScores : Html Msg
highScores =
    button
        [ class "start-button high-score"
        , onClick (ShowHighScores True)
        ]
        []


instructions : Html Msg
instructions =
    button
        [ class "start-button instructions"
        , onClick (ShowInstructions True)
        ]
        []


bombCount : Model -> Html Msg
bombCount model =
    div [ class "bomb-count" ]
        [ div [ class "bomb-icon" ] [], text (padLeftNum (Maybe.map .numberOfBombs model.game |> Maybe.withDefault 0)) ]


timer : Model -> Html Msg
timer model =
    div [ class "timer" ]
        [ div [ class "timer-icon" ] []
        , text (durationToSeconds model.duration)
        ]


header : Model -> Html Msg
header model =
    div [ class "header" ]
        [ startButton model
        , highScores
        , instructions
        , levelToggle model
        , bombCount model
        , timer model
        ]


youWin : Model -> Html Msg
youWin model =
    div [ class "modal" ]
        [ div [ class "modal-content" ]
            [ div [] [ text ("Congratulations! You won in " ++ durationToSeconds model.duration ++ " seconds") ]
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


instructionsModal : Bool -> String -> Html Msg
instructionsModal touch user =
    div [ class "modal" ]
        [ div [ class "modal-content" ]
            [ div []
                [ p [] [ text <| "Hello @" ++ user ]
                , if touch then
                    p [] [ text "Tap to reveal a square, long-press to (un)flag a square" ]

                  else
                    p [] [ text "Click to reveal a square, Ctrl or Cmd click to (un)flag a square" ]
                ]
            , Button.button "Got It" (ShowInstructions False)
            ]
        ]


score : Maybe Float -> String
score =
    Maybe.map durationToSeconds >> Maybe.withDefault "___"


fastestTimesModal : FastestTimes -> Html Msg
fastestTimesModal { easy, normal, hard, hardcore } =
    div [ class "modal" ]
        [ div [ class "modal-content" ]
            [ div [ class "fastest-times" ]
                [ h3 [] [ text "Fastest Times" ]
                , div [ class "time" ]
                    [ div [] [ text "Easy:" ], div [] [ text <| score easy ] ]
                , div [ class "time" ]
                    [ div [] [ text "Normal:" ], div [] [ text <| score normal ] ]
                , div [ class "time" ]
                    [ div [] [ text "Hard:" ], div [] [ text <| score hard ] ]
                , div [ class "time" ]
                    [ div [] [ text "Hardcore:" ], div [] [ text <| score hardcore ] ]
                ]
            , Button.button "Close" (ShowHighScores False)
            ]
        ]


root : Model -> Html Msg
root model =
    let
        level =
            model.flags.level

        instr =
            model.flags.instructions

        showHighScores =
            model.highScores

        fastestTimes =
            model.flags.fastestTimes

        user =
            model.flags.username
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
                (case model.game of
                    Nothing ->
                        []

                    Just gameState ->
                        List.range 0 (gameState.config.dimensions.rows - 1) |> List.map (drawRow model.flags.level model.flags.touch gameState.config gameState.grid)
                )
            , if instr then
                instructionsModal model.flags.touch user

              else
                text ""
            , if showHighScores then
                fastestTimesModal fastestTimes

              else
                text ""
            ]
        , case model.state of
            Won ->
                youWin model

            Lost ->
                youLose

            _ ->
                div [] []
        ]
