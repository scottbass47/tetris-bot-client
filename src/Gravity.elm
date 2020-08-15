module Gravity exposing (..)

import Array exposing (Array)
import Board exposing (Board)
import GameState exposing (GameState, spawnTetromino)
import Maybe exposing (withDefault)
import SRS exposing (tryMove)


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


doGravity : GameState -> GameState
doGravity state =
    case state.currTetromino of
        Nothing ->
            state

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
            in
            if state.gravityFrames >= gravityLevel then
                case tryMove state.board tetromino ( 0, -1 ) of
                    (Just _) as t ->
                        { state | currTetromino = t }
                            |> updateGravityFrames
                            |> doGravity

                    Nothing ->
                        { state
                            | board = Board.placeTetromino tetromino state.board
                            , currTetromino = Just (spawnTetromino ())
                        }
                            |> updateGravityFrames

            else
                state
