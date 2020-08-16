module GameState exposing (..)

import Board exposing (Board)
import PieceGen
import Random
import Tetromino exposing (Tetromino)
import Types exposing (GridPoint, Msg(..), Piece)


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
