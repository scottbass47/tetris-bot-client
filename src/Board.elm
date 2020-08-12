module Board exposing (..)

import Array exposing (Array)
import Maybe exposing (withDefault)
import Tetromino exposing (Mino(..), minos)
import Types exposing (GridPoint)


type alias Board =
    { arr : Array Mino
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


minoAt : Board -> GridPoint -> Mino
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
