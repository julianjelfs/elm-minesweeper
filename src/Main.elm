module Main exposing (init, keyBoard, main, timerSub)

import Browser
import Browser.Events as Events
import Json.Decode as Decode
import State
import Types exposing (..)
import View


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


timerSub : Model -> Sub Msg
timerSub model =
    case model.state of
        Playing ->
            Events.onAnimationFrameDelta Tick

        _ ->
            Sub.none


isCtrl : (Bool -> Msg) -> Decode.Decoder Msg
isCtrl tag =
    Decode.map (tag << isCtrlKey) (Decode.field "key" Decode.string)


isCtrlKey : String -> Bool
isCtrlKey =
    (==) "Control"


keyBoard : Sub Msg
keyBoard =
    Sub.batch
        [ Events.onKeyDown (isCtrl KeyDown)
        , Events.onKeyUp (isCtrl KeyUp)
        ]



--| WIRING


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = State.update
        , view = View.root
        , subscriptions = \m -> Sub.batch [ timerSub m, keyBoard ]
        }
