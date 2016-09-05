module State exposing (update)

import Types exposing (..)
import RandomPositions
import Dict
import Debug exposing (log)

ctrl = 17

replaceCell : Cell -> Dict.Dict Coord Cell -> Dict.Dict Coord Cell
replaceCell cell grid =
    Dict.update (cell.rowIndex, cell.cellIndex) (\mc ->
        case mc of
            Just c -> Just cell
            Nothing -> mc)  grid

translate grid cell (dx, dy) =
    Dict.get (dx cell.cellIndex, dy cell.rowIndex) grid

inc x =
    x + 1

dec x =
    x - 1

id x =
    x

{--
(-1,-1) (0, -1) (1, -1)
(-1, 0) (0,0)   (1, 0)
(-1, 1) (0, 1)  (1, 1)
--}
nearbyCells grid cell =
    [ (dec, dec)
    , (dec, id)
    , (dec, inc)
    , (id, dec)
    , (id, inc)
    , (inc, dec)
    , (inc, id)
    , (inc, inc) ]
        |> List.filterMap (translate grid cell)

findNearbyBombs nearby grid cell =
    case cell.nearbyBombs of
        Just n -> n
        Nothing ->
            nearby
                |> List.filter .bomb
                |> List.length

revealCell model cell =
    case cell.bomb of
        True ->
            { model | state = Lost
            , grid = model.grid
                |> (replaceCell { cell | state = Cleared }) }
        False ->
            let
                nearby = nearbyCells model.grid cell

                nearbyBombs =
                    findNearbyBombs nearby model.grid cell

                updatedModel =
                    { model | grid =
                        model.grid
                            |> (replaceCell { cell | state = Cleared
                             , nearbyBombs = Just nearbyBombs }) }
            in
                case nearbyBombs of
                    0 ->
                        -- if there are 0, show that cell and reveal all surrounding cells
                        updatedModel
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
        startGame = (\m ->
            ( { initialModel | state = Playing, ctrl = m.ctrl }, RandomPositions.get ))

        ctrlClick = (\m k b ->
            case k of
                ctrl ->
                    ( { m | ctrl = b }, Cmd.none ))

        handleClick = (\m c ->
            case m.ctrl of
                True ->
                    (flagCell m c)
                False ->
                    (revealCell m c) )
    in
        case msg of
            Dummy () ->
                (model, Cmd.none)

            Positions pos ->
                ({ model | grid = (addBombsToGrid pos model.grid) }, Cmd.none)

            StartGame ->
                startGame model

            Tick t ->
                ( { model | duration = model.duration + t }, Cmd.none )

            ClickedCell cell ->
                case model.state of
                    Playing ->
                        ( (handleClick model cell), Cmd.none )
                    _ ->
                        let
                            (m, fx) =
                                startGame model
                        in
                            ( (handleClick m cell), fx )

            KeyDown k ->
                ctrlClick model k True

            KeyUp k ->
                ctrlClick model k False
