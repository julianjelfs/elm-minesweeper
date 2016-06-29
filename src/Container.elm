module Container exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type Msg a =
    Click
    | ContentMsg a

type alias Model b =
    { content : b
    , num : Int
    }

initialModel : b -> Model b
initialModel init =
    Model init 0

update : Msg a -> Model b -> (Model b, Cmd a)
update msg model =
    case msg of
        Click ->
            ( { model | num = model.num + 1 }, Cmd.none )
        _ ->
            (model, Cmd.none)


view: Model b -> Html (Msg a)
view model =
    div
        []
        [ button
            [ onClick Click ]
            [ text "Container" ]
        , div
            []
            [ text (toString model.num) ]
        ]
