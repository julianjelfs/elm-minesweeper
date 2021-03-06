module View exposing (root)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (padLeft)
import Types exposing (..)


drawCell grid y x =
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
                            case cell.bomb of
                                True ->
                                    ( "cell cleared bomb", "" )

                                False ->
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
                ]
                [ text txt ]

        Nothing ->
            div [] []


drawRow grid y =
    div [ class "row" ]
        (List.range 0 9 |> List.map (drawCell grid y))


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


padLeftNum =
    String.fromInt >> padLeft 3 '0'


bombCount model =
    span [ class "bomb-count" ]
        [ text (padLeftNum model.numberOfBombs) ]


timer model =
    span [ class "timer" ]
        [ text (durationToSeconds model.duration) ]


header : Model -> Html Msg
header model =
    div [ class "header" ]
        [ bombCount model
        , startButton model
        , timer model
        ]


durationToSeconds =
    (\a -> (/) a 1000) >> round >> padLeftNum


youWin : Model -> Html Msg
youWin model =
    div [ class "you-win" ]
        [ div [] [ text ("Congratulations! You won in " ++ (model.duration |> durationToSeconds) ++ " seconds") ]
        , button
            [ class "restart"
            , onClick StartGame
            ]
            [ text "Start Again" ]
        ]


root : Model -> Html Msg
root model =
    div [ class "container" ]
        [ div [ class "instructions" ]
            [ text "Click to reveal a square, Ctrl-click to flag a square" ]
        , div [ class "game-area" ]
            [ header model
            , div [ class "grid" ]
                (List.range 0 9 |> List.map (drawRow model.grid))
            ]
        , div [ class "see-code" ]
            [ span []
                [ text "See the code "
                , a [ target "_blank", href "https://github.com/julianjelfs/elm-minesweeper" ] [ text "here" ]
                ]
            ]
        , case model.state of
            Won ->
                youWin model

            _ ->
                div [] []
        ]
