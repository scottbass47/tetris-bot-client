module Main exposing (main)

import Array
import Board exposing (Board, minoCount)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (Point, Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color exposing (Color)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Tetromino exposing (..)
import Types exposing (GridPoint, Rotation(..))


type alias Model =
    { count : Float
    , board : Board
    , currTetromino : Maybe Tetromino
    }


type Msg
    = Frame Float


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialTetromino =
            mkTetromino T |> moveTetromino ( 5, 10 ) |> rotate CW |> rotate CW
    in
    ( { count = 0, board = Board.initBoard boardDims, currTetromino = Just initialTetromino }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( round boardWidth, round boardHeight )
            []
            (renderGame model)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( { model | count = model.count + 1 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Frame


boardDims : GridPoint
boardDims =
    ( 20, 10 )


boardPosition : Point
boardPosition =
    ( 0, 0 )


minoSize : Float
minoSize =
    25


boardWidth : Float
boardWidth =
    boardDims |> Tuple.second |> toFloat |> (*) minoSize


boardHeight : Float
boardHeight =
    boardDims |> Tuple.first |> toFloat |> (*) minoSize


minoColor : Piece -> Color
minoColor mino =
    case mino of
        I ->
            Color.rgb255 18 240 240

        J ->
            Color.rgb255 0 0 240

        L ->
            Color.rgb255 240 160 1

        T ->
            Color.rgb255 160 0 240

        S ->
            Color.rgb255 22 240 0

        Z ->
            Color.rgb255 240 0 1

        O ->
            Color.rgb255 240 240 1

        Blank ->
            Color.rgb255 197 197 197


minoPosition : GridPoint -> Point
minoPosition ( r, c ) =
    let
        offX =
            Tuple.first boardPosition

        offY =
            Tuple.second boardPosition

        flipY y =
            boardHeight - y - minoSize
    in
    ( c |> toFloat |> (*) minoSize |> (+) offX, r |> toFloat |> (*) minoSize |> (+) offY |> flipY )


renderGame : Model -> List Renderable
renderGame model =
    let
        currTetrominoRender =
            case model.currTetromino of
                Nothing ->
                    []

                Just t ->
                    renderTetromino t
    in
    -- clearScreen :: currTetrominoRender ++ renderBoard model
    clearScreen :: renderBoard model ++ currTetrominoRender


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) boardWidth boardHeight ]


renderTetromino : Tetromino -> List Renderable
renderTetromino tetromino =
    List.map (renderMino tetromino.piece) (minosPositions tetromino)


renderMino : Piece -> GridPoint -> Renderable
renderMino mino pos =
    shapes [ fill (minoColor mino) ] [ rect (minoPosition pos) minoSize minoSize ]


renderBoard : Model -> List Renderable
renderBoard { board } =
    Array.indexedMap (\i mino -> renderMino mino (Board.fromArrIndex board i)) board.arr |> Array.toList
