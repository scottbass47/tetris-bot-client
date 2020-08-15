module Main exposing (main)

import Array
import Board exposing (Board, minoCount)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyPress, onKeyUp)
import Canvas exposing (Point, Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color exposing (Color)
import GameState exposing (GameState, initialGameState)
import Gravity exposing (doGravity)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as Json
import Keyboard.Event exposing (KeyCode, KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Keys exposing (Key)
import SRS exposing (tryMove)
import Tetromino exposing (..)
import Types exposing (GridPoint, Pos, Rotation(..))


type alias Model =
    GameState


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
    ( initialGameState ()
    , Cmd.none
    )


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
        Frame _ ->
            let
                newModel =
                    { model | gravityFrames = model.gravityFrames + 1 }
                        |> doGravity
            in
            ( newModel, Cmd.none )

        HandleInput input ->
            ( handleInput input.keyCode model, Cmd.none )


handleInput : Key -> Model -> Model
handleInput key model =
    let
        newModel =
            case key of
                Keys.Left ->
                    movePiece ( -1, 0 ) model

                Keys.Right ->
                    movePiece ( 1, 0 ) model

                Keys.Z ->
                    rotatePiece CW model

                Keys.X ->
                    rotatePiece CCW model

                Keys.Up ->
                    doHardDrop model

                _ ->
                    model
    in
    newModel


movePiece : Pos -> Model -> Model
movePiece delta model =
    case model.currTetromino of
        Nothing ->
            model

        Just t ->
            case SRS.tryMove model.board t delta of
                Nothing ->
                    model

                Just newT ->
                    { model | currTetromino = Just newT }


rotatePiece : Rotation -> Model -> Model
rotatePiece rotation model =
    case model.currTetromino of
        Nothing ->
            model

        Just t ->
            case SRS.tryRotate model.board t rotation of
                Nothing ->
                    model

                Just newT ->
                    { model | currTetromino = Just newT }


doHardDrop : Model -> Model
doHardDrop model =
    case model.currTetromino of
        Nothing ->
            model

        Just tetromino ->
            let
                f t =
                    case SRS.tryMove model.board t ( 0, -1 ) of
                        Nothing ->
                            { model | currTetromino = Just t }

                        Just newT ->
                            f newT
            in
            f tetromino


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onKeyDown (Json.map HandleInput decodeKeyboardEvent)
        ]


boardPosition : Point
boardPosition =
    ( 0, 0 )


minoSize : Float
minoSize =
    25


boardWidth : Float
boardWidth =
    GameState.boardDims |> Tuple.second |> toFloat |> (*) minoSize


boardHeight : Float
boardHeight =
    GameState.boardDims |> Tuple.first |> toFloat |> (*) minoSize


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
