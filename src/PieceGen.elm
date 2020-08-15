module PieceGen exposing (..)

import List
import Random
import Types exposing (Piece(..))


fullBag : List Piece
fullBag =
    [ I
    , J
    , L
    , T
    , S
    , Z
    , O
    ]


{-| Please don't call with an empty list thanks
-}
pieceGenerator : List Piece -> Random.Generator Piece
pieceGenerator bag =
    let
        hd =
            List.head bag |> Maybe.withDefault Blank

        tl =
            List.tail bag |> Maybe.withDefault []
    in
    Random.uniform hd tl
