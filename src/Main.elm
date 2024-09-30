module Main exposing (init, keyBoard, main, timerSub)

import Browser
import Browser.Events as Events
import Json.Decode as JD
import State
import Types exposing (..)
import View


flagsDecoder : JD.Decoder Flags
flagsDecoder =
    JD.map3 Flags
        (JD.field "username" JD.string)
        (JD.field "level" levelDecoder)
        (JD.field "dimensions" dimensionsDecoder)


dimensionsDecoder : JD.Decoder Dimensions
dimensionsDecoder =
    JD.map2 Dimensions
        (JD.field "width" JD.float)
        (JD.map (\h -> h - 163) (JD.field "height" JD.float))


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
                    { username = "Unknown", level = Normal, dimensions = { width = 0, height = 0 } }

                Ok decoded ->
                    decoded
    in
    ( initialModel flags, Cmd.none )


timerSub : Model -> Sub Msg
timerSub model =
    case model.state of
        Playing ->
            Events.onAnimationFrameDelta Tick

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
