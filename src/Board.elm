module Board exposing (..)

import Array exposing (Array)
import List
import Maybe exposing (withDefault)
import Tetromino exposing (Tetromino, minosPositions)
import Types exposing (GridPoint, Piece(..))


type alias Board =
    { arr : Array Piece
    , rows : Int
    , cols : Int
    }


initBoard : GridPoint -> Board
initBoard ( rows, cols ) =
    { arr = Array.repeat (rows * cols) Blank
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


placeTetromino : Tetromino -> Board -> Board
placeTetromino tetromino board =
    let
        folder p arr =
            Array.set (toArrIndex board p) tetromino.piece arr

        newArr =
            List.foldl folder board.arr (minosPositions tetromino)

        rowsToClear i arr =
            if i == board.rows then
                []

            else
                let
                    currentRow =
                        Array.slice (i * board.cols) ((i + 1) * board.cols) arr

                    filteredRow =
                        Array.filter (\m -> m == Blank) currentRow
                in
                if Array.length filteredRow == 0 then
                    i :: rowsToClear (i + 1) arr

                else
                    rowsToClear (i + 1) arr
    in
    { board | arr = newArr } |> clearRows (rowsToClear 0 newArr)


clearRows : List Int -> Board -> Board
clearRows toClear board =
    let
        numRowsToClear =
            List.length toClear

        newArr =
            board.arr
                |> Array.indexedMap (\i m -> ( fromArrIndex board i |> Tuple.first, m ))
                |> Array.filter (\( r, _ ) -> List.member r toClear |> not)
                |> Array.map Tuple.second

        newRows =
            Array.repeat (numRowsToClear * board.cols) Blank
    in
    { board | arr = Array.append newArr newRows }


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
