module Container exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type Msg msg =
    Click
    | ContentMsg msg

type alias Model m =
    { content : m
    , num : Int
    }

initialModel : m -> Model m
initialModel init =
    Model init 0

update : Msg msg -> Model m -> (Model m, Cmd msg)
update msg model =
    case msg of
        Click ->
            ( { model | num = model.num + 1 }, Cmd.none )
        _ ->
            (model, Cmd.none)


view: Model m -> (m -> Html (Msg msg)) -> Html (Msg mgs)
view model content =
    div
        []
        [ button
            [ onClick Click ]
            [ text "Container" ]
        , div
            []
            [ content model.content ]
        , div
            []
            [ text (toString model.num) ]
        ]
