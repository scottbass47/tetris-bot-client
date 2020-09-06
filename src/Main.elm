port module Main exposing (main)

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
import Http exposing (..)
import Json.Decode as Json
import Json.Encode as Encode
import Keyboard.Event exposing (decodeKeyboardEvent)
import Keyboard.Key as Keys exposing (Key)
import PieceGen
import Platform.Cmd exposing (Cmd)
import Tetromino exposing (..)
import Types exposing (GridPoint, Msg(..), Piece(..), Rotation(..))


type alias Model =
    { gameState : GameState
    , frameElapsed : Float
    , frameCounter : Int
    , fps : Int
    }


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


port socketOpen : (String -> msg) -> Sub msg


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
        state =
            initialGameState ()
    in
    ( { gameState = state
      , frameElapsed = 0
      , frameCounter = 0
      , fps = 0
      }
    , GameState.spawnTetromino state
    )


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( round (boardWidth model.gameState.board), round (boardHeight model.gameState.board) )
            []
            (renderGame model.gameState)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newGameState, cmd ) =
            updateGame msg model.gameState

        newModel =
            case msg of
                Frame dt ->
                    { model
                        | frameElapsed = model.frameElapsed + (dt / 1000)
                        , frameCounter = model.frameCounter + 1
                    }
                        |> (\m ->
                                if m.frameElapsed >= 1 then
                                    { m
                                        | frameElapsed = m.frameElapsed - 1
                                        , frameCounter = 0
                                        , fps = Debug.log "FPS: " m.frameCounter
                                    }

                                else
                                    m
                           )

                _ ->
                    model
    in
    ( { newModel | gameState = newGameState }, cmd )


updateGame : Msg -> GameState -> ( GameState, Cmd Msg )
updateGame msg state =
    case msg of
        Frame dt ->
            { state | gravityFrames = state.gravityFrames + 1 }
                |> GameState.tryIncrementLockDelay (dt / 1000)
                |> doGravity

        KeyDown input ->
            handleInput input.keyCode state

        KeyUp input ->
            let
                newState =
                    case input.keyCode of
                        Keys.Down ->
                            { state | softDropping = False }

                        _ ->
                            state
            in
            ( newState, Cmd.none )

        NextPiece piece ->
            let
                newTetromino =
                    piece |> mkTetromino |> moveTetromino ( 4, 20 )

                newBag =
                    List.filter (not << (==) piece) state.pieceBag
                        |> (\b ->
                                if List.isEmpty b then
                                    PieceGen.fullBag

                                else
                                    b
                           )

                newState =
                    { state | currTetromino = Just newTetromino, pieceBag = newBag }
            in
            ( newState, Cmd.none )

        SocketOpen _ ->
            ( state, sendInitialData state.board )

        ServerMsg str ->
            ( state, sendMessage (Debug.log "Sending: " ("lmao " ++ str)) )


sendInitialData : Board -> Cmd msg
sendInitialData board =
    let
        body =
            Encode.object [ ( "type", Encode.string "initial" ), ( "data", encodeBoardSize board ) ]
    in
    sendMessage (Encode.encode 0 body)


encodeBoardSize : Board -> Encode.Value
encodeBoardSize board =
    let
        ( rows, cols ) =
            Board.boardDims board
    in
    Encode.object [ ( "boardSize", Encode.array Encode.int (Array.fromList [ rows, cols ]) ) ]


handleInput : Key -> GameState -> ( GameState, Cmd Msg )
handleInput key state =
    case key of
        Keys.Left ->
            GameState.movePiece ( -1, 0 ) state

        Keys.Right ->
            GameState.movePiece ( 1, 0 ) state

        Keys.Z ->
            GameState.rotatePiece CW state

        Keys.X ->
            GameState.rotatePiece CCW state

        Keys.Up ->
            GameState.doHardDrop state

        Keys.Down ->
            GameState.doSoftDrop state

        _ ->
            ( state, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onKeyDown (Json.map KeyDown decodeKeyboardEvent)
        , onKeyUp (Json.map KeyUp decodeKeyboardEvent)
        , messageReceiver ServerMsg
        , socketOpen SocketOpen
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


renderGame : GameState -> List Renderable
renderGame state =
    let
        currTetrominoRender =
            case state.currTetromino of
                Nothing ->
                    []

                Just t ->
                    renderTetromino state t
    in
    clearScreen state :: renderBoard state ++ currTetrominoRender


clearScreen : GameState -> Renderable
clearScreen state =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) (boardWidth state.board) (boardHeight state.board) ]


renderTetromino : GameState -> Tetromino -> List Renderable
renderTetromino state tetromino =
    List.map (renderMino state tetromino.piece) (minosPositions tetromino)


renderMino : GameState -> Piece -> GridPoint -> Renderable
renderMino state mino pos =
    shapes [ fill (minoColor mino) ] [ rect (minoPosition state.board pos) minoSize minoSize ]


renderBoard : GameState -> List Renderable
renderBoard ({ board } as state) =
    let
        ( r, c ) =
            boardRenderDims board

        shouldRender ( rr, cc ) =
            rr >= 0 && rr < r && cc >= 0 && cc < c
    in
    board.arr
        |> Array.toIndexedList
        |> List.filter (\( i, _ ) -> shouldRender (Board.fromArrIndex board i))
        |> List.map (\( i, mino ) -> renderMino state mino (Board.fromArrIndex board i))
