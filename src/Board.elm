module Board exposing (..)

import Array exposing (Array)
import List
import Maybe exposing (withDefault)
import Tetromino exposing (Piece(..), Tetromino, minos, minosPositions)
import Types exposing (GridPoint, Pos)


type alias Board =
    { arr : Array Piece
    , rows : Int
    , cols : Int
    }


initBoard : GridPoint -> Board
initBoard ( rows, cols ) =
    let
        minoArr =
            Array.fromList minos

        length =
            Array.length minoArr

        indexToMino i =
            i
                |> Basics.modBy length
                |> (\idx -> Array.get idx minoArr)
                |> withDefault Blank
    in
    { arr = Array.initialize (rows * cols) indexToMino
    , rows = rows
    , cols = cols
    }


minoAt : Board -> GridPoint -> Piece
minoAt board pos =
    Array.get (toArrIndex board pos) board.arr |> withDefault Blank


toArrIndex : Board -> GridPoint -> Int
toArrIndex board ( r, c ) =
    r * board.cols + c


fromArrIndex : Board -> Int -> GridPoint
fromArrIndex board i =
    ( i // board.cols, Basics.modBy board.cols i )


boardDims : Board -> GridPoint
boardDims board =
    ( board.rows, board.cols )


minoCount : Board -> Int
minoCount board =
    board.rows * board.cols


canPlace : Board -> Tetromino -> Bool
canPlace board tetromino =
    let
        minos =
            minosPositions tetromino
    in
    List.all (\mino -> inBounds board mino && minoAt board mino == Blank) minos


inBounds : Board -> GridPoint -> Bool
inBounds board ( r, c ) =
    r >= 0 && r < board.rows && c >= 0 && c < board.cols
