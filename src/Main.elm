module Main exposing (init, keyBoard, main, timerSub)

import Browser
import Browser.Events as Events
import Json.Decode as JD
import State
import Task
import Types exposing (..)
import View


flagsDecoder : JD.Decoder Flags
flagsDecoder =
    JD.map3 Flags
        (JD.field "username" JD.string)
        (JD.field "level" levelDecoder)
        (JD.field "instructions" JD.bool)


levelDecoder : JD.Decoder Level
levelDecoder =
    JD.int
        |> JD.map
            (\l ->
                case l of
                    0 ->
                        Easy

                    2 ->
                        Hard

                    _ ->
                        Normal
            )


init : JD.Value -> ( Model, Cmd Msg )
init args =
    let
        flags =
            case JD.decodeValue flagsDecoder args of
                Err _ ->
                    { username = "Unknown", level = Normal, instructions = False }

                Ok decoded ->
                    decoded
    in
    ( Initialising flags, Task.perform (\_ -> GetDimensions) (Task.succeed ()) )


timerSub : Model -> Sub Msg
timerSub model =
    case model of
        Initialised m ->
            case m.state of
                Playing ->
                    Events.onAnimationFrameDelta Tick

                _ ->
                    Sub.none

        _ ->
            Sub.none


isCtrl : (Bool -> Msg) -> JD.Decoder Msg
isCtrl tag =
    JD.map (tag << isCtrlKey) (JD.field "key" JD.string)


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


main : Program JD.Value Model Msg
main =
    Browser.element
        { init = init
        , update = State.update
        , view = View.root
        , subscriptions = \m -> Sub.batch [ timerSub m, keyBoard ]
        }
