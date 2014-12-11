import Graphics.Collage (..)
import Graphics.Element (..)
import Mouse
import Signal
import Text
import Time (..)
import Window

type Input = { mouseIsDown:Bool , delta:Time }
data Level = ClickTheScreenToContinue | ToDoLevel
type Game = { level:Level }

input : Signal Input
input = sampleOn delta <| Input <~ Mouse.isDown
                                 ~ delta

delta : Signal Time
delta = inSeconds <~ fps 35

initialGame : Game
initialGame = { level = ClickTheScreenToContinue }

stepGame : Input -> Game -> Game
stepGame {mouseIsDown, delta} ({level} as game) =
    case level of
        ClickTheScreenToContinue ->
            if mouseIsDown then {game | level <- ToDoLevel} else game
        ToDoLevel -> game

gameState : Signal Game
gameState = foldp stepGame initialGame input

txt f = leftAligned << f << monospace << Text.color black << toText

-- display a game state
display : (Int,Int) -> Game -> Element
display (width, height) {level} =
      container width height middle <|
      collage width height
       [ toForm <| txt (Text.height <| (toFloat height) / 20) <|
               show level
       ]

main : Signal Element
--main = S.map asText gameState
main = lift2 display Window.dimensions gameState
