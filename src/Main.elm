module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (Point, Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color exposing (Color)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Maybe exposing (withDefault)


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
        minoArr =
            Array.fromList minos

        length =
            Array.length minoArr

        indexToMino i =
            i
                |> Basics.modBy length
                |> (\idx -> Array.get idx minoArr)
                |> withDefault Blank
    in
    ( { count = 0, board = Array.initialize (rows * cols) indexToMino }, Cmd.none )


type alias Board =
    Array Mino


type alias Model =
    { count : Float
    , board : Board
    }


type Msg
    = Frame Float


type alias GridPoint =
    ( Int, Int )


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


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) boardWidth boardHeight ]


boardDims : ( Int, Int )
boardDims =
    ( 20, 10 )


rows =
    Tuple.first boardDims


cols =
    Tuple.second boardDims


boardPosition : Point
boardPosition =
    ( 0, 0 )


minoSize =
    25


toArrIndex : GridPoint -> Int
toArrIndex ( r, c ) =
    r * cols + c


fromArrIndex : Int -> GridPoint
fromArrIndex i =
    ( i // cols, Basics.modBy cols i )


minoAt : Board -> GridPoint -> Mino
minoAt arr pos =
    Array.get (toArrIndex pos) arr |> withDefault Blank


boardWidth : Float
boardWidth =
    boardDims |> Tuple.second |> (*) minoSize |> toFloat


boardHeight : Float
boardHeight =
    boardDims |> Tuple.first |> (*) minoSize |> toFloat


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


renderMino : Mino -> GridPoint -> Renderable
renderMino mino pos =
    shapes [ fill (minoColor mino) ] [ rect (minoPosition pos) minoSize minoSize ]


renderBoard : Model -> List Renderable
renderBoard { count, board } =
    let
        transformIdx i =
            i |> (+) (round (count / 5)) |> Basics.modBy (rows * cols)
    in
    Array.indexedMap (\i mino -> renderMino mino (i |> transformIdx |> fromArrIndex)) board |> Array.toList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( { model | count = model.count + 1 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Frame
