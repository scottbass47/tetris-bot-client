module Tetromino exposing (Mino(..), minos)


type Mino
    = I
    | J
    | L
    | T
    | S
    | Z
    | O
    | Blank


minos : List Mino
minos =
    [ I, J, L, T, S, Z, O, Blank ]
