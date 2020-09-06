module Messages exposing (..)

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder)
import Types exposing (..)


type IncomingMsg
    = MakeMove MakeMoveD


type alias MakeMoveD =
    { inputs : List Move
    }


type alias Move =
    { input : Input
    , position : GridPoint
    , orientation : Orientation
    }


incomingMsgDecoder : Decoder IncomingMsg
incomingMsgDecoder =
    let
        dataDecoder =
            \str ->
                let
                    d =
                        case str of
                            "makeMove" ->
                                Decode.map MakeMove makeMoveDecoder

                            _ ->
                                Decode.fail "Invalid message type"
                in
                Decode.field "data" d
    in
    Decode.field "type" Decode.string
        |> Decode.andThen dataDecoder


makeMoveDecoder : Decoder MakeMoveD
makeMoveDecoder =
    Decode.list moveDecoder
        |> Decode.field "inputs"
        |> Decode.map MakeMoveD


moveDecoder : Decoder Move
moveDecoder =
    Decode.map3 Move inputDecoder positionDecoder orientationDecoder


positionDecoder : Decoder GridPoint
positionDecoder =
    Decode.map2 Tuple.pair (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)


inputDecoder : Decoder Input
inputDecoder =
    Decode.field "input" Decode.string
        |> Decode.andThen inputFromString


inputFromString : String -> Decoder Input
inputFromString str =
    case str of
        "moveLeft" ->
            Decode.succeed MoveLeft

        "moveRight" ->
            Decode.succeed MoveRight

        "rotateCW" ->
            Decode.succeed (Rotate CW)

        "rotateCCW" ->
            Decode.succeed (Rotate CCW)

        "softDrop" ->
            Decode.succeed SoftDrop

        "hardDrop" ->
            Decode.succeed HardDrop

        _ ->
            Decode.fail "Invalid input"


orientationDecoder : Decoder Orientation
orientationDecoder =
    Decode.field "orientation" Decode.string
        |> Decode.andThen orientationFromString


orientationFromString : String -> Decoder Orientation
orientationFromString str =
    case str of
        "right" ->
            Decode.succeed Right

        "left" ->
            Decode.succeed Left

        "two" ->
            Decode.succeed Two

        "zero" ->
            Decode.succeed Zero

        _ ->
            Decode.fail "Invalid orientation"
