module Button exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


button : String -> msg -> Html msg
button txt fn =
    Html.button
        [ class "btn"
        , onClick fn
        ]
        [ text txt]
