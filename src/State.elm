module State exposing (update)

import Types exposing (..)
import RandomPositions
import Dict
import Debug exposing (log)

ctrl = 17

replaceCell cell grid =
    Dict.update (cell.x, cell.y) (\mc -> Maybe.map (\c -> cell) mc) grid

revealCell model grid cell =
    case cell.bomb of
        True ->
            { model | state = Lost
            , grid = grid |> (replaceCell { cell | state = Cleared }) }
        False ->
            let
                updatedModel =
                    { model | grid =
                        grid |> (replaceCell { cell | state = Cleared }) }
            in
                case cell.nearbyBombs of
                    Just 0 ->
                        -- if there are 0, show that cell and reveal all surrounding cells
                        let
                            nearby =
                                nearbyCells updatedModel.grid (cell.x, cell.y)
                                    |> List.filter (\c -> c.state == Hidden)

                            grid = List.foldl (\c g -> (revealCell model g c) |> .grid) updatedModel.grid nearby
                        in
                            { updatedModel | grid = grid }
                    _ ->
                        -- if there are > 0, show that cell
                        updatedModel

flagCell model cell =
    let
        changeState = (\b s ->
            { model | numberOfBombs = b
            , grid = model.grid |> (replaceCell { cell | state = s }) })
    in
        case cell.state of
            Flagged ->
                changeState (model.numberOfBombs + 1) Hidden
            Hidden ->
                changeState (model.numberOfBombs - 1) Flagged
            _ ->
                model


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        startGame = (\m c ->
            ( { initialModel | state = Playing
                , ctrl = m.ctrl
                , cellClicked = c }, RandomPositions.get ))

        ctrlClick = (\m k b ->
            case k of
                ctrl ->
                    ( { m | ctrl = b }, Cmd.none ))

        handleClick = (\m c ->
            case m.ctrl of
                True ->
                    (flagCell m c)
                False ->
                    (revealCell m m.grid c) )
    in
        case msg of
            Dummy () ->
                (model, Cmd.none)

            Positions pos ->
                let
                    grid = addBombsToGrid pos model.grid
                    cellClicked = Maybe.andThen model.cellClicked (\coord -> getCell grid coord)
                    m = { model | cellClicked = Nothing, grid = grid }
                in
                    case cellClicked of
                        Nothing ->
                            (m, Cmd.none)
                        Just c ->
                            ( (handleClick m c), Cmd.none )

            StartGame ->
                startGame model Nothing

            Tick t ->
                ( { model | duration = model.duration + t }, Cmd.none )

            ClickedCell cell ->
                case model.state of
                    Playing ->
                        ( (handleClick model cell), Cmd.none )
                    _ ->
                        startGame model (Just (cell.x, cell.y))

            KeyDown k ->
                ctrlClick model k True

            KeyUp k ->
                ctrlClick model k False