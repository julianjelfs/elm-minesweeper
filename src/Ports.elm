port module Ports exposing (..)


port updateLevel : Int -> Cmd msg


port instructions : Bool -> Cmd msg


port resize : (Bool -> msg) -> Sub msg
