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
    }


spawnTetromino : GameState -> Cmd Msg
spawnTetromino state =
    Random.generate NextPiece
        (PieceGen.pieceGenerator state.pieceBag)
