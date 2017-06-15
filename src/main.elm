module Main exposing (..)

import Color exposing (..)
import Html exposing (..)
import Keyboard
import Random
import Time exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Types exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


cellWidth : Int
cellWidth =
    30


cellHeight : Int
cellHeight =
    30


nodeGap : Int
nodeGap =
    1


foodGap : Int
foodGap =
    5


foodLifeInTicks : Int
foodLifeInTicks =
    10


type Direction
    = LEFT
    | RIGHT
    | UP
    | DOWN


type alias Ticks =
    Int


type alias CellPosX =
    Int


type alias CellPosY =
    Int


type alias Cell =
    ( CellPosX, CellPosY )


type alias SnakeBody =
    List Cell


type alias Food =
    ( CellPosX, CellPosY, Ticks )


type alias Dirty =
    Bool


type alias DirtyDirection =
    ( Direction, Dirty )


type alias Model =
    { rows : Int
    , cols : Int
    , tickTime : Time
    , dirtyDirection : DirtyDirection
    , snakeBody : SnakeBody
    , food : Food
    }


initModel : Model
initModel =
    { rows = 10
    , cols = 10
    , tickTime = second
    , dirtyDirection = ( RIGHT, False )
    , snakeBody = [ ( 2, 0 ), ( 1, 0 ), ( 0, 0 ) ]
    , food = ( 5, 5, foodLifeInTicks )
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , generateNewFood
    )


generateNewFood : Cmd Msg
generateNewFood =
    Random.generate NewFood (Random.pair (Random.int 0 (initModel.cols - 1)) (Random.int 0 (initModel.rows - 1)))


type Msg
    = Tick
    | KeyDown Keyboard.KeyCode
    | NewFood ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model
                | snakeBody = moveSnake model.dirtyDirection model.snakeBody model.rows model.cols
                , dirtyDirection = ( Tuple.first model.dirtyDirection, False )
                , food = decrementFoodTick model.food
              }
            , if isFoodTickZero model.food then
                generateNewFood
              else
                Cmd.none
            )

        KeyDown code ->
            case getNewDirection model.dirtyDirection code of
                Nothing ->
                    ( model, Cmd.none )

                Just newDirection ->
                    ( { model | dirtyDirection = ( newDirection, True ) }, Cmd.none )

        NewFood ( cellX, cellY ) ->
            ( { model | food = ( cellX, cellY, foodLifeInTicks ) }, Cmd.none )


isFoodTickZero : Food -> Bool
isFoodTickZero ( _, _, tick ) =
    tick == 0


decrementFoodTick : Food -> Food
decrementFoodTick ( posx, posy, tick ) =
    ( posx, posy, tick - 1 )


getNewDirection : DirtyDirection -> Keyboard.KeyCode -> Maybe Direction
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


getNewHeadPos : Maybe Cell -> Direction -> Int -> Int -> Maybe Cell
getNewHeadPos currPos direction maxRows maxCols =
    let
        newHeadPos diffCellX diffCellY =
            case currPos of
                Just ( currCellPosX, currCellPosY ) ->
                    let
                        newCellPosX =
                            currCellPosX + diffCellX

                        newCellPosY =
                            currCellPosY + diffCellY

                        finalCellPosX =
                            if newCellPosX >= maxCols then
                                0
                            else if newCellPosX < 0 then
                                maxCols - 1
                            else
                                newCellPosX

                        finalCellPosY =
                            if newCellPosY >= maxRows then
                                0
                            else if newCellPosY < 0 then
                                maxRows - 1
                            else
                                newCellPosY
                    in
                    Just ( finalCellPosX, finalCellPosY )

                Nothing ->
                    Nothing
    in
    case direction of
        LEFT ->
            newHeadPos -1 0

        RIGHT ->
            newHeadPos 1 0

        UP ->
            newHeadPos 0 -1

        DOWN ->
            newHeadPos 0 1


moveSnake : DirtyDirection -> SnakeBody -> Int -> Int -> SnakeBody
moveSnake ( direction, _ ) snakeBody maxRows maxCols =
    let
        maybeNewHead =
            getNewHeadPos (List.head snakeBody) direction maxRows maxCols
    in
    case maybeNewHead of
        Just newHead ->
            [ newHead ] ++ List.take (List.length snakeBody - 1) snakeBody

        Nothing ->
            snakeBody


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every model.tickTime (\_ -> Tick), Keyboard.downs (\keycode -> KeyDown keycode) ]


view : Model -> Html Msg
view model =
    let
        gameWidth =
            model.cols * cellWidth

        gameHeight =
            model.rows * cellHeight
    in
    div []
        [ svg [ viewBox 0 0 gameWidth gameHeight, width (px (toFloat gameWidth)), height (px (toFloat gameHeight)) ]
            (drawGameElements model.snakeBody model.food)
        , pre [] [ text (toString model) ]
        ]


drawGameElements : SnakeBody -> Food -> List (Html Msg)
drawGameElements snakeBody food =
    drawSnake snakeBody ++ [ drawFood food ]


drawSnake : SnakeBody -> List (Html Msg)
drawSnake snakeBody =
    List.map drawSnakeNode snakeBody


drawSnakeNode : Cell -> Html Msg
drawSnakeNode ( cellPosX, cellPosY ) =
    drawNode cellPosX cellPosY nodeGap Color.black


drawFood : Food -> Html Msg
drawFood ( cellPosX, cellPosY, _ ) =
    drawNode cellPosX cellPosY foodGap Color.green


drawNode : CellPosX -> CellPosY -> Int -> Color -> Html Msg
drawNode cellPosX cellPosY gap color =
    let
        posX =
            cellPosX * cellWidth

        posY =
            cellPosY * cellHeight
    in
    rect
        [ x (px (toFloat (posX + gap)))
        , y (px (toFloat (posY + gap)))
        , width (px (toFloat (cellWidth - 2 * gap)))
        , height (px (toFloat (cellHeight - 2 * gap)))
        , fill color
        ]
        []
