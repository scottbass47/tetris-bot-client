module GameState exposing (..)

import Board exposing (Board)
import Tetromino exposing (Piece(..), Tetromino, mkTetromino, moveTetromino)
import Types exposing (GridPoint)


type alias GameState =
    { board : Board
    , currTetromino : Maybe Tetromino
    , gravityFrames : Float
    , level : Int
    , softDropping : Bool
    }


boardDims : GridPoint
boardDims =
    ( 22, 10 )


initialGameState : () -> GameState
initialGameState () =
    { board = Board.initBoard boardDims
    , currTetromino = Just (spawnTetromino ())
    , gravityFrames = 0
    , level = 0
    , softDropping = False
    }


spawnTetromino : () -> Tetromino
spawnTetromino () =
    mkTetromino T |> moveTetromino ( 5, 17 )
