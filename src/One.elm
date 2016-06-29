module One exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type Msg =
    Click

type alias Model = Int

initialModel : Model
initialModel = 0

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Click ->
            ( model + 1, Cmd.none )

view: Model -> Html Msg
view model =
    div
        []
        [ button
            [ onClick Click ]
            [ text "Click me!" ]
        ]
