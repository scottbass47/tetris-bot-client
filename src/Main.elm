module Main exposing (main)

import Array
import Board exposing (Board)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Canvas exposing (Point, Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color exposing (Color)
import GameState exposing (GameState, boardDims, initialGameState)
import Gravity exposing (doGravity)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as Json
import Keyboard.Event exposing (decodeKeyboardEvent)
import Keyboard.Key as Keys exposing (Key)
import PieceGen
import SRS exposing (tryMove)
import Tetromino exposing (..)
import Types exposing (GridPoint, Msg(..), Piece(..), Pos, Rotation(..))


type alias Model =
    GameState


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
    let
        model =
            initialGameState ()
    in
    ( model
    , GameState.spawnTetromino model
    )


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( round (boardWidth model.board), round (boardHeight model.board) )
            []
            (renderGame model)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            { model | gravityFrames = model.gravityFrames + 1 }
                |> doGravity

        KeyDown input ->
            ( handleInput input.keyCode model, Cmd.none )

        KeyUp input ->
            let
                newModel =
                    case input.keyCode of
                        Keys.Down ->
                            { model | softDropping = False }

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        NextPiece piece ->
            let
                newTetromino =
                    piece |> mkTetromino |> moveTetromino ( 5, 17 )

                newBag =
                    List.filter (not << (==) piece) model.pieceBag
                        |> (\b ->
                                if List.isEmpty b then
                                    PieceGen.fullBag

                                else
                                    b
                           )

                newModel =
                    { model | currTetromino = Just newTetromino, pieceBag = newBag }
            in
            ( newModel, Cmd.none )


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

                Keys.Down ->
                    doSoftDrop model

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


doSoftDrop : Model -> Model
doSoftDrop model =
    { model
        | softDropping = True
        , gravityFrames =
            if not model.softDropping then
                0

            else
                model.gravityFrames
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onKeyDown (Json.map KeyDown decodeKeyboardEvent)
        , onKeyUp (Json.map KeyUp decodeKeyboardEvent)
        ]


boardPosition : Point
boardPosition =
    ( 0, 0 )


boardRenderDims : Board -> GridPoint
boardRenderDims board =
    let
        ( r, c ) =
            Board.boardDims board
    in
    ( r - 2, c )


minoSize : Float
minoSize =
    25


boardWidth : Board -> Float
boardWidth board =
    board |> boardRenderDims |> Tuple.second |> toFloat |> (*) minoSize


boardHeight : Board -> Float
boardHeight board =
    board |> boardRenderDims |> Tuple.first |> toFloat |> (*) minoSize


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


minoPosition : Board -> GridPoint -> Point
minoPosition board ( r, c ) =
    let
        offX =
            Tuple.first boardPosition

        offY =
            Tuple.second boardPosition

        flipY y =
            boardHeight board - y - minoSize
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
                    renderTetromino model t
    in
    clearScreen model :: renderBoard model ++ currTetrominoRender


clearScreen : Model -> Renderable
clearScreen model =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) (boardWidth model.board) (boardHeight model.board) ]


renderTetromino : Model -> Tetromino -> List Renderable
renderTetromino model tetromino =
    List.map (renderMino model tetromino.piece) (minosPositions tetromino)


renderMino : Model -> Piece -> GridPoint -> Renderable
renderMino model mino pos =
    shapes [ fill (minoColor mino) ] [ rect (minoPosition model.board pos) minoSize minoSize ]


renderBoard : Model -> List Renderable
renderBoard ({ board } as model) =
    let
        ( r, c ) =
            boardRenderDims board

        shouldRender ( rr, cc ) =
            rr >= 0 && rr < r && cc >= 0 && cc < c
    in
    board.arr
        |> Array.toIndexedList
        |> List.filter (\( i, _ ) -> shouldRender (Board.fromArrIndex board i))
        |> List.map (\( i, mino ) -> renderMino model mino (Board.fromArrIndex board i))
