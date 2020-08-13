module Main exposing (main)

import Array
import Board exposing (Board, minoCount)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyPress)
import Canvas exposing (Point, Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color exposing (Color)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as Json
import Keyboard.Event exposing (KeyCode, KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Keys exposing (Key)
import SRS exposing (tryMove)
import Tetromino exposing (..)
import Types exposing (GridPoint, Rotation(..))


type alias Model =
    { board : Board
    , currTetromino : Maybe Tetromino
    , gravityElapsed : Float
    , gravityThreshold : Float
    }


type Msg
    = Frame Float
    | HandleInput KeyboardEvent


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { board = Board.initBoard boardDims
      , currTetromino = Just (spawnTetromino ())
      , gravityElapsed = 0
      , gravityThreshold = 200
      }
    , Cmd.none
    )


spawnTetromino : () -> Tetromino
spawnTetromino () =
    mkTetromino T |> moveTetromino ( 5, 17 )


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( round boardWidth, round boardHeight )
            []
            (renderGame model)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame dt ->
            let
                newModel =
                    { model | gravityElapsed = model.gravityElapsed + dt }
                        |> doGravity
            in
            ( newModel, Cmd.none )

        HandleInput input ->
            ( handleInput input.keyCode model, Cmd.none )


handleInput : Key -> Model -> Model
handleInput key model =
    let
        delta =
            case key of
                Keys.Left ->
                    ( -1, 0 )

                Keys.Right ->
                    ( 1, 0 )

                _ ->
                    ( 0, 0 )

        rotation =
            case key of
                Keys.Z ->
                    Just CW

                Keys.X ->
                    Just CCW

                _ ->
                    Nothing

        srsResult =
            model.currTetromino
                |> Maybe.andThen (\t -> SRS.tryMove model.board t delta)
                |> Maybe.andThen
                    (\t ->
                        case rotation of
                            Nothing ->
                                Just t

                            Just r ->
                                SRS.tryRotate model.board t r
                    )

        newTetromino =
            case srsResult of
                Nothing ->
                    model.currTetromino

                _ as t ->
                    t
    in
    { model | currTetromino = newTetromino }


doGravity : Model -> Model
doGravity model =
    case model.currTetromino of
        Nothing ->
            model

        Just tetromino ->
            if model.gravityElapsed >= model.gravityThreshold then
                let
                    newModel =
                        case tryMove model.board tetromino ( 0, -1 ) of
                            (Just _) as t ->
                                { model | currTetromino = t }

                            Nothing ->
                                { model
                                    | board = Board.placeTetromino tetromino model.board
                                    , currTetromino = Just (spawnTetromino ())
                                }
                in
                { newModel | gravityElapsed = newModel.gravityElapsed - newModel.gravityThreshold }

            else
                model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onKeyDown (Json.map HandleInput decodeKeyboardEvent)
        ]


boardDims : GridPoint
boardDims =
    ( 20, 10 )


boardPosition : Point
boardPosition =
    ( 0, 0 )


minoSize : Float
minoSize =
    25


boardWidth : Float
boardWidth =
    boardDims |> Tuple.second |> toFloat |> (*) minoSize


boardHeight : Float
boardHeight =
    boardDims |> Tuple.first |> toFloat |> (*) minoSize


minoColor : Piece -> Color
minoColor mino =
    case mino of
        I ->
            Color.rgb255 18 240 240

        J ->
            Color.rgb255 0 0 240

        L ->
            Color.rgb255 240 160 1

        T ->
            Color.rgb255 160 0 240

        S ->
            Color.rgb255 22 240 0

        Z ->
            Color.rgb255 240 0 1

        O ->
            Color.rgb255 240 240 1

        Blank ->
            Color.rgb255 197 197 197


minoPosition : GridPoint -> Point
minoPosition ( r, c ) =
    let
        offX =
            Tuple.first boardPosition

        offY =
            Tuple.second boardPosition

        flipY y =
            boardHeight - y - minoSize
    in
    ( c |> toFloat |> (*) minoSize |> (+) offX, r |> toFloat |> (*) minoSize |> (+) offY |> flipY )


renderGame : Model -> List Renderable
renderGame model =
    let
        currTetrominoRender =
            case model.currTetromino of
                Nothing ->
                    []

                Just t ->
                    renderTetromino t
    in
    -- clearScreen :: currTetrominoRender ++ renderBoard model
    clearScreen :: renderBoard model ++ currTetrominoRender


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) boardWidth boardHeight ]


renderTetromino : Tetromino -> List Renderable
renderTetromino tetromino =
    List.map (renderMino tetromino.piece) (minosPositions tetromino)


renderMino : Piece -> GridPoint -> Renderable
renderMino mino pos =
    shapes [ fill (minoColor mino) ] [ rect (minoPosition pos) minoSize minoSize ]


renderBoard : Model -> List Renderable
renderBoard { board } =
    Array.indexedMap (\i mino -> renderMino mino (Board.fromArrIndex board i)) board.arr |> Array.toList
