module Tetromino exposing (..)

import List
import Maybe exposing (withDefault)
import Types exposing (GridPoint, Pos, Rotation(..))


type Piece
    = I
    | J
    | L
    | T
    | S
    | Z
    | O
    | Blank


type Orientation
    = Zero
    | Right
    | Two
    | Left


type alias Tetromino =
    { piece : Piece
    , orientation : Orientation
    , pos : Pos
    , localPos : List Pos
    , offsets : List ( Orientation, List Pos )
    }


minos : List Piece
minos =
    [ I, J, L, T, S, Z, O, Blank ]


getOffset : Orientation -> Tetromino -> List Pos
getOffset orientation t =
    t.offsets
        |> List.filter (\( o, _ ) -> o == orientation)
        |> List.head
        |> withDefault ( Zero, [] )
        |> Tuple.second


minosPositions : Tetromino -> List GridPoint
minosPositions tetromino =
    let
        ( px, py ) =
            tetromino.pos

        o =
            tetromino.orientation

        rotatedMinos =
            List.map (rotateMino o) tetromino.localPos

        shiftedMinos =
            List.map (\( x, y ) -> ( x + px, y + py )) rotatedMinos
    in
    List.map (\( x, y ) -> ( y, x )) shiftedMinos


mkTetromino : Piece -> Tetromino
mkTetromino piece =
    let
        positions =
            initialPositions piece

        offsets =
            pieceOffsets piece
    in
    Tetromino piece Zero ( 0, 0 ) positions offsets


rotateMino : Orientation -> Pos -> Pos
rotateMino orientation ( x, y ) =
    case orientation of
        Zero ->
            ( x, y )

        Right ->
            ( y, -x )

        Two ->
            ( -x, -y )

        Left ->
            ( -y, x )


rotate : Rotation -> Tetromino -> Tetromino
rotate dir =
    case dir of
        CW ->
            rotateCW

        CCW ->
            rotateCCW


rotateCW : Tetromino -> Tetromino
rotateCW tetromino =
    let
        newOrientation =
            case tetromino.orientation of
                Zero ->
                    Right

                Right ->
                    Two

                Two ->
                    Left

                Left ->
                    Zero
    in
    rotateTetromino newOrientation tetromino


rotateCCW : Tetromino -> Tetromino
rotateCCW tetromino =
    let
        newOrientation =
            case tetromino.orientation of
                Zero ->
                    Left

                Left ->
                    Two

                Two ->
                    Right

                Right ->
                    Zero
    in
    rotateTetromino newOrientation tetromino


moveTetromino : Pos -> Tetromino -> Tetromino
moveTetromino ( x, y ) t =
    { t | pos = ( x + Tuple.first t.pos, y + Tuple.second t.pos ) }


rotateTetromino : Orientation -> Tetromino -> Tetromino
rotateTetromino o tetromino =
    { tetromino | orientation = o }


initialPositions : Piece -> List Pos
initialPositions piece =
    case piece of
        I ->
            [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]

        J ->
            [ ( -1, 1 ), ( -1, 0 ), ( 0, 0 ), ( 1, 0 ) ]

        L ->
            [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 1, 1 ) ]

        T ->
            [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 0, 1 ) ]

        S ->
            [ ( -1, 0 ), ( 0, 0 ), ( 0, 1 ), ( 1, 1 ) ]

        Z ->
            [ ( -1, 1 ), ( 0, 1 ), ( 0, 0 ), ( 1, 0 ) ]

        O ->
            [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ]

        Blank ->
            []


pieceOffsets : Piece -> List ( Orientation, List Pos )
pieceOffsets piece =
    case piece of
        I ->
            [ ( Zero, [ ( 0, 0 ), ( -1, 0 ), ( 2, 0 ), ( -1, 0 ), ( 2, 0 ) ] )
            , ( Right, [ ( -1, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 1 ), ( 0, -2 ) ] )
            , ( Two, [ ( -1, 1 ), ( 1, 1 ), ( -2, 1 ), ( 1, 0 ), ( -2, 0 ) ] )
            , ( Left, [ ( 0, 1 ), ( 0, 1 ), ( 0, 1 ), ( 0, -1 ), ( 0, 2 ) ] )
            ]

        O ->
            [ ( Zero, [ ( 0, 0 ) ] )
            , ( Right, [ ( 0, -1 ) ] )
            , ( Two, [ ( -1, -1 ) ] )
            , ( Left, [ ( -1, 0 ) ] )
            ]

        _ ->
            [ ( Zero, [ ( 0, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ) ] )
            , ( Right, [ ( 0, 0 ), ( 1, 0 ), ( 1, -1 ), ( 0, 2 ), ( 1, 2 ) ] )
            , ( Two, [ ( 0, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ) ] )
            , ( Left, [ ( 0, 0 ), ( -1, 0 ), ( -1, -1 ), ( 0, 2 ), ( -1, 2 ) ] )
            ]
