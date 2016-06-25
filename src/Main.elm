module Main exposing(..)

import Html.App as Html
import Html exposing (..)
import Debug exposing (log)

type alias Model = 
    { test: String
    }

type Msg =
    NoOp

initialModel: Model
initialModel =
    Model ""

-- START APP
init : ( Model, Cmd Msg )
init =
  ( initialModel, Cmd.none )

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

view: Model -> Html Msg
view model =
    div 
        []
        [ text "Nice from minesweeper, does this work? Probably not" ]

main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\m -> Sub.none)
        }
