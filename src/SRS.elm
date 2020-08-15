module SRS exposing (tryMove, tryRotate)

import Board exposing (Board, canPlace)
import Tetromino exposing (Tetromino, getOffset, moveTetromino, rotate)
import Types exposing (Orientation, Pos, Rotation)


tryRotate : Board -> Tetromino -> Rotation -> Maybe Tetromino
tryRotate board tetromino dir =
    let
        t =
            rotate dir tetromino

        orientations =
            ( tetromino.orientation, t.orientation )

        tOffsets =
            getOffsets t orientations
    in
    tryOffsets board t tOffsets


getOffsets : Tetromino -> ( Orientation, Orientation ) -> List Pos
getOffsets tetromino ( from, to ) =
    let
        fromOffsets =
            getOffset from tetromino

        toOffsets =
            getOffset to tetromino
    in
    List.map2 (\( fx, fy ) ( tx, ty ) -> ( fx - fy, tx - ty )) fromOffsets toOffsets


tryOffsets : Board -> Tetromino -> List Pos -> Maybe Tetromino
tryOffsets board tetromino =
    let
        f maybeT lst =
            case ( maybeT, lst ) of
                ( Just t, _ ) ->
                    Just t

                ( _, [] ) ->
                    Nothing

                ( _, x :: xs ) ->
                    if tryOffset board tetromino x then
                        Just (moveTetromino x tetromino)

                    else
                        f Nothing xs
    in
    f Nothing


tryOffset : Board -> Tetromino -> Pos -> Bool
tryOffset board tetromino offset =
    tetromino |> moveTetromino offset |> canPlace board


tryMove : Board -> Tetromino -> Pos -> Maybe Tetromino
tryMove board tetromino delta =
    let
        t =
            moveTetromino delta tetromino
    in
    if canPlace board t then
        Just t

    else
        Nothing
