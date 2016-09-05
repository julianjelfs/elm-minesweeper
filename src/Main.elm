module Main exposing(..)

import Html.App as Html
import Task
import AnimationFrame exposing (diffs)
import Keyboard
import Types exposing (..)
import View
import State

init : ( Model, Cmd Msg )
init =
  ( initialModel, Cmd.none )

timerSub model =
    case model.state of
        Playing ->
            diffs Tick
        _ -> Sub.none

keyBoard =
    Sub.batch [Keyboard.downs KeyDown, Keyboard.ups KeyUp]

--WIRING
main =
    Html.program
        { init = init
        , update = State.update
        , view = View.root
        , subscriptions = (\m -> Sub.batch [timerSub m, keyBoard])
        }
