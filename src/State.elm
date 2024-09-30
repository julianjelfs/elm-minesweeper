module State exposing (update)

import Browser.Dom as Dom
import Debug exposing (log)
import Dict
import Html.Attributes exposing (start)
import Ports
import RandomPositions
import Task
import Types exposing (..)


replaceCell : Cell -> Grid -> Grid
replaceCell cell grid =
    Dict.insert ( cell.x, cell.y ) cell grid


revealCell : GameState -> Grid -> Cell -> GameState
revealCell model grid cell =
    if cell.bomb then
        { model
            | state = Lost
            , grid = replaceCell { cell | state = Cleared } grid
        }

    else
        let
            updatedModel =
                { model
                    | grid =
                        replaceCell { cell | state = Cleared } grid
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


flagCell : GameState -> Cell -> GameState
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


startGame : GameState -> Maybe Coord -> ( GameState, Cmd Msg )
startGame model coord =
    let
        init =
            initialModel { username = model.username, level = model.level } (Just model.dimensions)
    in
    case init of
        Initialising _ ->
            ( model, Cmd.none )

        Initialised gameState ->
            ( { gameState
                | state = Playing
                , ctrl = model.ctrl
                , cellClicked = coord
              }
            , Cmd.batch
                [ RandomPositions.get gameState.config
                , Ports.updateLevel (levelToInt model.level)
                ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case m of
        Initialising flags ->
            case msg of
                GetDimensions ->
                    ( m, Dom.getElement "game-grid" |> Task.map (\dom -> { width = dom.element.width, height = dom.element.height }) |> Task.attempt GotDimensions )

                GotDimensions (Ok dimensions) ->
                    ( initialModel flags (Just dimensions), Cmd.none )

                GotDimensions (Err _) ->
                    ( m, Cmd.none )

                _ ->
                    ( m, Cmd.none )

        Initialised model ->
            let
                handleClick =
                    \gameState c ->
                        let
                            updated =
                                if gameState.ctrl then
                                    flagCell gameState c

                                else
                                    revealCell gameState gameState.grid c

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
            (case msg of
                Positions pos ->
                    let
                        grid =
                            addBombsToGrid pos model.grid

                        cellClicked =
                            Maybe.andThen (\coord -> getCell coord grid) model.cellClicked

                        updated =
                            { model | cellClicked = Nothing, grid = grid }
                    in
                    case cellClicked of
                        Nothing ->
                            ( updated, Cmd.none )

                        Just c ->
                            ( handleClick updated c, Cmd.none )

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

                KeyDown _ ->
                    ( { model | ctrl = True }, Cmd.none )

                KeyUp _ ->
                    ( { model | ctrl = False }, Cmd.none )

                ToggleLevel ->
                    let
                        level =
                            case model.level of
                                Easy ->
                                    Normal

                                Normal ->
                                    Hard

                                Hard ->
                                    Hardcore

                                Hardcore ->
                                    Easy
                    in
                    startGame { model | level = level } Nothing

                _ ->
                    ( model, Cmd.none )
            )
                |> (\( gs, cmds ) -> ( Initialised gs, cmds ))
