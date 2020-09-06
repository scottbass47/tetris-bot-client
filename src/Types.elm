module Types exposing (..)

import Keyboard.Event exposing (KeyboardEvent)


type alias GridPoint =
    ( Int, Int )


type alias Pos =
    ( Int, Int )


type Rotation
    = CW
    | CCW


type Input
    = MoveLeft
    | MoveRight
    | HardDrop
    | SoftDrop
    | Rotate Rotation


type Msg
    = Frame Float
    | KeyDown KeyboardEvent
    | KeyUp KeyboardEvent
    | NextPiece Piece
    | ServerMsg String
    | SocketOpen String


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
