module State exposing (update)

import Browser.Dom as Dom
import Debug exposing (log)
import Dict
import Html.Attributes exposing (start)
import Json.Encode as JE
import Ports
import Process
import RandomPositions
import Set
import String exposing (replace)
import Task
import Types exposing (..)


replaceCell : Cell -> Grid -> Grid
replaceCell cell grid =
    Dict.insert ( cell.x, cell.y ) cell grid


processQueue : Model -> List Cell -> Model
processQueue model queue =
    case model.game of
        Nothing ->
            model

        Just game ->
            case queue of
                [] ->
                    model

                cell :: rest ->
                    let
                        updatedGame =
                            { game | grid = replaceCell { cell | state = Cleared } game.grid }

                        updatedModel =
                            { model | game = Just updatedGame }
                    in
                    if cell.state /= Hidden then
                        processQueue model rest

                    else if cell.bomb then
                        { updatedModel | state = Lost }

                    else
                        case cell.nearbyBombs of
                            Just 0 ->
                                let
                                    queueAsSet =
                                        Set.fromList (List.map (\c -> ( c.x, c.y )) rest)

                                    nearby =
                                        nearbyCells updatedGame.grid ( cell.x, cell.y )
                                            |> List.filter (\c -> c.state == Hidden && (not <| Set.member ( c.x, c.y ) queueAsSet))

                                    updatedQueue =
                                        List.append rest nearby
                                in
                                processQueue updatedModel updatedQueue

                            _ ->
                                processQueue updatedModel rest


revealCell : Model -> Cell -> Model
revealCell model cell =
    processQueue model [ cell ]


flagCell : Model -> Cell -> Model
flagCell model cell =
    case model.game of
        Nothing ->
            model

        Just game ->
            let
                changeState =
                    \b s ->
                        { game
                            | numberOfBombs = b
                            , grid = game.grid |> replaceCell { cell | state = s }
                        }
            in
            (case cell.state of
                Flagged ->
                    changeState (game.numberOfBombs + 1) Hidden

                Hidden ->
                    if game.numberOfBombs > 0 then
                        changeState (game.numberOfBombs - 1) Flagged

                    else
                        game

                _ ->
                    game
            )
                |> (\g -> { model | game = Just g })


noHiddenCells =
    Dict.filter (\k v -> v.state == Hidden) >> Dict.isEmpty


startGame : Model -> Maybe Coord -> ( Model, Cmd Msg )
startGame model coord =
    let
        init =
            initialModel model.flags (Maybe.map .dimensions model.game)
    in
    case init.game of
        Nothing ->
            ( model, Cmd.none )

        Just game ->
            ( { init
                | state = Playing
                , ctrl = model.ctrl
                , cellClicked = coord
              }
            , Cmd.batch
                [ RandomPositions.get game.config
                , Ports.updateLevel (levelToInt model.flags.level)
                ]
            )


encodeOptionalFloat : Maybe Float -> JE.Value
encodeOptionalFloat f =
    case f of
        Nothing ->
            JE.null

        Just f_ ->
            JE.float f_


encodeFastestTimes : FastestTimes -> JE.Value
encodeFastestTimes { easy, normal, hard, hardcore } =
    JE.object
        [ ( "easy", encodeOptionalFloat easy )
        , ( "normal", encodeOptionalFloat normal )
        , ( "hard", encodeOptionalFloat hard )
        , ( "hardcore", encodeOptionalFloat hardcore )
        ]


updateFastestTimes : Model -> FastestTimes
updateFastestTimes model =
    let
        { level, fastestTimes } =
            model.flags

        duration =
            model.duration
    in
    case level of
        Easy ->
            case fastestTimes.easy of
                Nothing ->
                    { fastestTimes | easy = Just duration }

                Just easy ->
                    if easy > duration then
                        { fastestTimes | easy = Just duration }

                    else
                        fastestTimes

        Normal ->
            case fastestTimes.normal of
                Nothing ->
                    { fastestTimes | normal = Just duration }

                Just normal ->
                    if normal > duration then
                        { fastestTimes | normal = Just duration }

                    else
                        fastestTimes

        Hard ->
            case fastestTimes.hard of
                Nothing ->
                    { fastestTimes | hard = Just duration }

                Just hard ->
                    if hard > duration then
                        { fastestTimes | hard = Just duration }

                    else
                        fastestTimes

        Hardcore ->
            case fastestTimes.hardcore of
                Nothing ->
                    { fastestTimes | hardcore = Just duration }

                Just hardcore ->
                    if hardcore > duration then
                        { fastestTimes | hardcore = Just duration }

                    else
                        fastestTimes


refreshFastestTimes : Model -> ( Model, Cmd Msg )
refreshFastestTimes model =
    let
        updatedTimes =
            updateFastestTimes model

        flags =
            model.flags

        updatedFlags =
            { flags | fastestTimes = updatedTimes }
    in
    ( { model | flags = updatedFlags }, Ports.updateFastest (encodeFastestTimes <| updatedTimes) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        handleClick =
            \m c ->
                let
                    updatedModel =
                        if m.ctrl then
                            flagCell m c

                        else
                            revealCell m c

                    won =
                        case updatedModel.game of
                            Nothing ->
                                False

                            Just game ->
                                (updatedModel.state == Playing)
                                    && (game.numberOfBombs == 0)
                                    && noHiddenCells game.grid
                in
                if won == True then
                    let
                        ( refreshed, cmd ) =
                            refreshFastestTimes updatedModel
                    in
                    ( { refreshed | state = Won }, cmd )

                else
                    ( updatedModel, Cmd.none )
    in
    case msg of
        StartPress cell ->
            ( { model | pressed = Just cell }
            , Task.perform (always LongPressed) (Task.succeed () |> Task.andThen (\_ -> Process.sleep model.threshold))
            )

        MouseMove ->
            ( { model | pressed = Nothing }, Cmd.none )

        EndPress ->
            case model.pressed of
                Nothing ->
                    ( model, Cmd.none )

                Just cell ->
                    let
                        ( updated, cmd ) =
                            handleClick model cell
                    in
                    ( { updated | pressed = Nothing }, cmd )

        LongPressed ->
            case model.pressed of
                Nothing ->
                    ( model, Cmd.none )

                Just cell ->
                    let
                        ( updated, cmd ) =
                            handleClick { model | ctrl = True } cell
                    in
                    ( { updated | ctrl = False, pressed = Nothing }, cmd )

        Resize ->
            ( model, Task.perform (\_ -> GetDimensions) (Task.succeed ()) )

        GetDimensions ->
            ( model, Dom.getElement "game-grid" |> Task.map (\dom -> { width = dom.element.width, height = dom.element.height }) |> Task.attempt GotDimensions )

        GotDimensions (Ok dimensions) ->
            ( initialModel model.flags (Just dimensions), Cmd.none )

        GotDimensions (Err _) ->
            ( model, Cmd.none )

        ShowHighScores show ->
            ( { model | highScores = show }, Cmd.none )

        ShowInstructions show ->
            let
                flags =
                    model.flags

                updatedFlags =
                    { flags | instructions = show }
            in
            ( { model | flags = updatedFlags }, Ports.instructions show )

        Positions pos ->
            case model.game of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    let
                        grid =
                            addBombsToGrid pos game.grid

                        cellClicked =
                            Maybe.andThen (\coord -> getCell coord grid) model.cellClicked

                        updatedGame =
                            { game | grid = grid }

                        updated =
                            { model | cellClicked = Nothing, game = Just updatedGame }
                    in
                    case cellClicked of
                        Nothing ->
                            ( updated, Cmd.none )

                        Just c ->
                            handleClick updated c

        StartGame ->
            startGame model Nothing

        Tick t ->
            ( { model | duration = model.duration + t }, Cmd.none )

        ClickedCell cell ->
            case model.state of
                Playing ->
                    handleClick model cell

                _ ->
                    startGame model (Just ( cell.x, cell.y ))

        KeyDown _ ->
            ( { model | ctrl = True }, Cmd.none )

        KeyUp _ ->
            ( { model | ctrl = False }, Cmd.none )

        ToggleLevel ->
            let
                level =
                    case model.flags.level of
                        Easy ->
                            Normal

                        Normal ->
                            Hard

                        Hard ->
                            Hardcore

                        Hardcore ->
                            Easy

                flags =
                    model.flags

                updatedFlags =
                    { flags | level = level }
            in
            startGame { model | flags = updatedFlags } Nothing
