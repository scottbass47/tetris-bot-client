module Gravity exposing (..)

import Array exposing (Array)
import Board
import GameState exposing (GameState, resetLockDelay, spawnIfReady, spawnTetromino, startLockDelay)
import Maybe exposing (withDefault)
import SRS exposing (tryMove)
import Tetromino exposing (Tetromino)
import Types exposing (Msg)


type alias GravityTable =
    Array Float


gravityTable : GravityTable
gravityTable =
    Array.fromList
        [ 60
        , 50
        , 40
        , 30
        , 20
        , 10
        , 8
        , 6
        , 4
        , 2
        , 1
        ]


softDropTable : GravityTable
softDropTable =
    Array.fromList
        [ 3
        , 3
        , 2
        , 2
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        , 1
        ]


doGravity : GameState -> ( GameState, Cmd Msg )
doGravity state =
    case state.currTetromino of
        Nothing ->
            ( state, Cmd.none )

        Just tetromino ->
            let
                gravityLevel =
                    Array.get state.level
                        (if state.softDropping then
                            softDropTable

                         else
                            gravityTable
                        )
                        |> withDefault 0

                updateGravityFrames : GameState -> GameState
                updateGravityFrames s =
                    { s | gravityFrames = s.gravityFrames - gravityLevel }

                doGravityHelper : Cmd Msg -> GameState -> ( GameState, Cmd Msg )
                doGravityHelper cmd st =
                    if st.gravityFrames >= gravityLevel then
                        case tryMove st.board tetromino ( 0, -1 ) of
                            (Just _) as t ->
                                { st | currTetromino = t }
                                    |> updateGravityFrames
                                    |> doGravityHelper cmd

                            Nothing ->
                                st
                                    |> startLockDelay
                                    |> updateGravityFrames
                                    |> spawnIfReady tetromino False

                    else
                        ( st, cmd )
            in
            doGravityHelper Cmd.none state

