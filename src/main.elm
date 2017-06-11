module Main exposing (..)

import Html exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Types exposing (..)
import Color exposing (..)
import Time exposing (..)
import Keyboard
import Random


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


cellWidth =
    30


foodLifeLength =
    10


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
    , food : ( Float, Float, Int )
    }


initModel =
    { rows = 10
    , cols = 10
    , tickTime = second
    , dirtyDirection = ( DOWN, False )
    , snakebody = [ ( 0, 2 ), ( 0, 1 ), ( 0, 0 ) ]
    , food = ( 5, 5, foodLifeLength )
    }


init =
    ( initModel
    , generateNewFood
    )


generateNewFood =
    (Random.generate NewFood (Random.pair (Random.int 0 (initModel.cols - 1)) (Random.int 0 (initModel.rows - 1))))


type Msg
    = Tick
    | KeyDown Keyboard.KeyCode
    | NewFood ( Int, Int )


update msg model =
    case msg of
        Tick ->
            ( { model
                | snakebody = (moveSnake model.dirtyDirection model.snakebody model.rows model.cols)
                , dirtyDirection = ( (Tuple.first model.dirtyDirection), False )
                , food = decrementFoodTick model.food
              }
            , if (isFoodTickZero model.food) then
                generateNewFood
              else
                Cmd.none
            )

        KeyDown code ->
            case (getNewDirection model.dirtyDirection code) of
                Nothing ->
                    ( model, Cmd.none )

                Just newDirection ->
                    ( { model | dirtyDirection = ( newDirection, True ) }, Cmd.none )

        NewFood ( posx, posy ) ->
            ( { model | food = ( (toFloat posx), (toFloat posy), foodLifeLength ) }, Cmd.none )


isFoodTickZero ( _, _, tick ) =
    tick == 0


decrementFoodTick ( posx, posy, tick ) =
    ( posx, posy, tick - 1 )


getNewDirection ( direction, dirty ) code =
    if dirty then
        Nothing
    else if (code == 37) && (direction /= RIGHT) && (direction /= LEFT) then
        Just LEFT
    else if (code == 39) && (direction /= LEFT) && (direction /= RIGHT) then
        Just RIGHT
    else if (code == 38) && (direction /= DOWN) && (direction /= UP) then
        Just UP
    else if (code == 40) && (direction /= UP) && (direction /= DOWN) then
        Just DOWN
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
        div []
            [ svg [ viewBox 0 0 gameWidth gameHeight, width (px gameWidth), height (px gameHeight) ]
                (drawBoard model.snakebody model.food)
            , pre [] [ text (toString model) ]
            ]


drawBoard snakebody food =
    (drawSnake snakebody) ++ [ (drawFood food) ]


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
            , width (px (cellWidth - 2 * gap))
            , height (px (cellWidth - 2 * gap))
            , fill Color.black
            ]
            []


drawFood ( posx, posy, _ ) =
    let
        newPosX =
            posx * cellWidth

        newPosY =
            posy * cellWidth

        gap =
            5
    in
        rect
            [ x (px (newPosX + gap))
            , y (px (newPosY + gap))
            , width (px (cellWidth - 2 * gap))
            , height (px (cellWidth - 2 * gap))
            , fill Color.green
            ]
            []
