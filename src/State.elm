module State exposing (update)

import Config exposing (config)
import Debug exposing (log)
import Dict
import RandomPositions
import Types exposing (..)


replaceCell cell grid =
    Dict.update ( cell.x, cell.y ) (Maybe.map (\c -> cell)) grid


revealCell model grid cell =
    case cell.bomb of
        True ->
            { model
                | state = Lost
                , grid = grid |> replaceCell { cell | state = Cleared }
            }

        False ->
            let
                updatedModel =
                    { model
                        | grid =
                            grid |> replaceCell { cell | state = Cleared }
                    }
            in
            case cell.nearbyBombs of
                Just 0 ->
                    -- if there are 0, show that cell and reveal all surrounding cells
                    let
                        nearby =
                            nearbyCells updatedModel.grid ( cell.x, cell.y )
                                |> List.filter (\c -> c.state == Hidden)

                        grid_ =
                            List.foldl (\c g -> revealCell model g c |> .grid) updatedModel.grid nearby
                    in
                    { updatedModel | grid = grid_ }

                _ ->
                    -- if there are > 0, show that cell
                    updatedModel


flagCell model cell =
    let
        changeState =
            \b s ->
                { model
                    | numberOfBombs = b
                    , grid = model.grid |> replaceCell { cell | state = s }
                }
    in
    case cell.state of
        Flagged ->
            changeState (model.numberOfBombs + 1) Hidden

        Hidden ->
            if model.numberOfBombs > 0 then
                changeState (model.numberOfBombs - 1) Flagged

            else
                model

        _ ->
            model


noHiddenCells =
    Dict.filter (\k v -> v.state == Hidden) >> Dict.isEmpty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        startGame =
            \m c ->
                ( { initialModel
                    | state = Playing
                    , ctrl = m.ctrl
                    , cellClicked = c
                  }
                , RandomPositions.get
                )

        handleClick =
            \m c ->
                let
                    updated =
                        case m.ctrl of
                            True ->
                                flagCell m c

                            False ->
                                revealCell m m.grid c

                    won =
                        (updated.state == Playing)
                            && (updated.numberOfBombs == 0)
                            && noHiddenCells updated.grid
                in
                if won == True then
                    { updated | state = Won }

                else
                    updated
    in
    case msg of
        Positions pos ->
            let
                grid =
                    addBombsToGrid pos model.grid

                cellClicked =
                    Maybe.andThen (\coord -> getCell coord grid) model.cellClicked

                m =
                    { model | cellClicked = Nothing, grid = grid }
            in
            case cellClicked of
                Nothing ->
                    ( m, Cmd.none )

                Just c ->
                    ( handleClick m c, Cmd.none )

        StartGame ->
            startGame model Nothing

        Tick t ->
            ( { model | duration = model.duration + t }, Cmd.none )

        ClickedCell cell ->
            case model.state of
                Playing ->
                    ( handleClick model cell, Cmd.none )

                _ ->
                    startGame model (Just ( cell.x, cell.y ))

        KeyDown isCtrl ->
            ( { model | ctrl = True }, Cmd.none )

        KeyUp isCtrl ->
            ( { model | ctrl = False }, Cmd.none )
