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
import Types exposing (GridPoint)


type alias Model =
    { count : Float
    , board : Board
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
    ( { count = 0, board = Board.initBoard boardDims }, Cmd.none )


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
            (clearScreen
                :: renderBoard model
            )
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


minoColor : Mino -> Color
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
            Color.white


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


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) boardWidth boardHeight ]


renderMino : Mino -> GridPoint -> Renderable
renderMino mino pos =
    shapes [ fill (minoColor mino) ] [ rect (minoPosition pos) minoSize minoSize ]


renderBoard : Model -> List Renderable
renderBoard { count, board } =
    let
        size =
            Board.minoCount board

        transformIdx i =
            i |> (+) (round (count / 5)) |> Basics.modBy size
    in
    Array.indexedMap (\i mino -> renderMino mino (i |> transformIdx |> Board.fromArrIndex board)) board.arr |> Array.toList
