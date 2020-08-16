module GameState exposing (..)

import Board exposing (Board)
import PieceGen
import Random
import SRS
import Tetromino exposing (Tetromino)
import Types exposing (GridPoint, Msg(..), Piece, Pos, Rotation)


type alias GameState =
    { board : Board
    , currTetromino : Maybe Tetromino
    , gravityFrames : Float
    , level : Int
    , softDropping : Bool
    , pieceBag : List Piece
    , lockDelay : Float
    , lockElapsed : Float
    , locking : Bool
    }


boardDims : GridPoint
boardDims =
    ( 22, 10 )


initialGameState : () -> GameState
initialGameState () =
    { board = Board.initBoard boardDims
    , currTetromino = Nothing
    , gravityFrames = 0
    , level = 0
    , softDropping = False
    , pieceBag = PieceGen.fullBag
    , lockDelay = 0.5
    , lockElapsed = 0
    , locking = False
    }


spawnTetromino : GameState -> Cmd Msg
spawnTetromino state =
    Random.generate NextPiece
        (PieceGen.pieceGenerator state.pieceBag)


movePiece : Pos -> GameState -> ( GameState, Cmd Msg )
movePiece delta state =
    let
        newState =
            case state.currTetromino of
                Nothing ->
                    state

                Just t ->
                    case SRS.tryMove state.board t delta of
                        Nothing ->
                            state

                        Just newT ->
                            { state | currTetromino = Just newT } |> resetLockDelay
    in
    ( newState, Cmd.none )


rotatePiece : Rotation -> GameState -> ( GameState, Cmd Msg )
rotatePiece rotation state =
    let
        newState =
            case state.currTetromino of
                Nothing ->
                    state

                Just t ->
                    case SRS.tryRotate state.board t rotation of
                        Nothing ->
                            state

                        Just newT ->
                            { state | currTetromino = Just newT } |> resetLockDelay
    in
    ( newState, Cmd.none )


doHardDrop : GameState -> ( GameState, Cmd Msg )
doHardDrop model =
    case model.currTetromino of
        Nothing ->
            ( model, Cmd.none )

        Just tetromino ->
            let
                f t =
                    case SRS.tryMove model.board t ( 0, -1 ) of
                        Nothing ->
                            spawnIfReady t True model

                        Just newT ->
                            f newT
            in
            f tetromino


doSoftDrop : GameState -> ( GameState, Cmd Msg )
doSoftDrop state =
    ( { state
        | softDropping = True
        , gravityFrames =
            if not state.softDropping then
                0

            else
                state.gravityFrames
      }
    , Cmd.none
    )


resetLockDelay : GameState -> GameState
resetLockDelay state =
    { state | lockElapsed = 0, locking = False }


tryIncrementLockDelay : Float -> GameState -> GameState
tryIncrementLockDelay dt state =
    if state.locking then
        { state | lockElapsed = state.lockElapsed + dt }

    else
        state



-- Starts the lock delay, resetting the timer if locking was previously false


startLockDelay : GameState -> GameState
startLockDelay state =
    { state | locking = True }


spawnIfReady : Tetromino -> Bool -> GameState -> ( GameState, Cmd Msg )
spawnIfReady tetromino ignoreLockDelay state =
    if state.lockElapsed >= state.lockDelay || ignoreLockDelay then
        { state | board = Board.placeTetromino tetromino state.board, currTetromino = Nothing }
            |> resetLockDelay
            |> (\s -> ( s, spawnTetromino s ))

    else
        ( state, Cmd.none )
