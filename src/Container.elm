module Container exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type Msg a =
    ContentMsg a

type alias Model b =
    { content : b
    }

initialModel : b -> Model b
initialModel init =
    Model init

update : Msg a -> Model b -> (Model b, Cmd a)
update msg model =
    (model, Cmd.none)

view: Model b -> Html (Msg a)
view model =
    div
        []
        [ button
            []
            [ text "Container" ]
        ]
