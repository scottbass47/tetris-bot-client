module Types exposing (..)


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
