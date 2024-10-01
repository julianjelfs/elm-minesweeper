port module Ports exposing (..)

import Json.Encode as JE


port updateLevel : Int -> Cmd msg


port instructions : Bool -> Cmd msg


port updateFastest : JE.Value -> Cmd msg


port resize : (Bool -> msg) -> Sub msg
