module Main exposing (..)

import Html exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Types exposing (..)
import Color exposing (..)
import Time exposing (..)
import Keyboard


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


cellWidth =
    30


type SnakeDirection
    = LEFT
    | RIGHT
    | UP
    | DOWN


type alias SnakeNode =
    ( Int, Int )


type alias Dirty =
    Bool


type alias Model =
    { rows : Int
    , cols : Int
    , tickTime : Float
    , dirtyDirection : ( SnakeDirection, Dirty )
    , snakebody : List SnakeNode
    }


initModel =
    { rows = 10
    , cols = 10
    , tickTime = second
    , dirtyDirection = ( DOWN, False )
    , snakebody = [ ( 0, 2 ), ( 0, 1 ), ( 0, 0 ) ]
    }


init =
    ( initModel
    , Cmd.none
    )


type Msg
    = Tick
    | KeyDown Keyboard.KeyCode


update msg model =
    case msg of
        Tick ->
            ( { model
                | snakebody = (moveSnake model.dirtyDirection model.snakebody model.rows model.cols)
                , dirtyDirection = ( (Tuple.first model.dirtyDirection), False )
              }
            , Cmd.none
            )

        KeyDown code ->
            case (getNewDirection model.dirtyDirection code) of
                Nothing ->
                    ( model, Cmd.none )

                Just newDirection ->
                    ( { model | dirtyDirection = ( newDirection, True ) }, Cmd.none )


getNewDirection ( direction, dirty ) code =
    if (dirty) then
        Nothing
    else if (code == 37) then
        if (direction /= RIGHT) && (direction /= LEFT) then
            Just LEFT
        else
            Nothing
    else if (code == 39) then
        if (direction /= LEFT) && (direction /= RIGHT) then
            Just RIGHT
        else
            Nothing
    else if (code == 38) then
        if (direction /= DOWN) && (direction /= UP) then
            Just UP
        else
            Nothing
    else if (code == 40) then
        if (direction /= UP) && (direction /= DOWN) then
            Just DOWN
        else
            Nothing
    else
        Nothing


moveSnake ( direction, _ ) snakebody maxRows maxCols =
    let
        newHead pos ( a, b ) =
            case pos of
                Just ( x, y ) ->
                    let
                        nx =
                            x + a

                        ny =
                            y + b

                        nx1 =
                            if (nx >= maxCols) then
                                0
                            else if (nx < 0) then
                                (maxCols - 1)
                            else
                                nx

                        ny1 =
                            if (ny >= maxRows) then
                                0
                            else if (ny < 0) then
                                (maxRows - 1)
                            else
                                ny
                    in
                        ( nx1, ny1 )

                Nothing ->
                    ( 0, 0 )

        newSnakeBody ( a, b ) =
            [ newHead (List.head snakebody) ( a, b ) ] ++ (List.take ((List.length snakebody) - 1) snakebody)
    in
        case direction of
            LEFT ->
                newSnakeBody ( 0, -1 )

            RIGHT ->
                newSnakeBody ( 0, 1 )

            UP ->
                newSnakeBody ( -1, 0 )

            DOWN ->
                newSnakeBody ( 1, 0 )


subscriptions model =
    Sub.batch [ Time.every model.tickTime (\_ -> Tick), Keyboard.downs (\keycode -> KeyDown keycode) ]


view model =
    let
        gameWidth =
            model.cols * cellWidth

        gameHeight =
            model.rows * cellWidth
    in
        svg [ viewBox 0 0 gameWidth gameHeight, width (px gameWidth), height (px gameHeight) ]
            (drawSnake model.snakebody)


drawSnake snakebody =
    List.map drawSnakeNode snakebody


drawSnakeNode ( row, col ) =
    let
        nodePosX =
            col * cellWidth

        nodePosY =
            row * cellWidth

        gap =
            1
    in
        rect
            [ x (px (nodePosX + gap))
            , y (px (nodePosY + gap))
            , width (px (cellWidth - gap))
            , height (px (cellWidth - gap))
            , fill Color.black
            ]
            []
