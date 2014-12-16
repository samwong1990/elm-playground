import Random
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Mouse
import Signal (..)
import Text
import Color
import Maybe
import Time (..)
import Window
import Debug(log)

{- top level types -}
type alias Game = {
      level:Level
    , globalSeed : Random.Seed
    }

type Level = ClickTheScreenToContinue | ToDoLevel ToDoLevelState

{- top level signals -}
type alias Input = {
      mouseIsDown:Bool
    , arrowKeyPressed:ArrowDirection
    }
input : Signal Input
input = dropRepeats <| Input <~ Mouse.isDown
                              ~ ( map (toArrowDirection << .x) Keyboard.arrows)

toArrowDirection : Int -> ArrowDirection
toArrowDirection x = case x of
    -1 -> Left
    1 -> Right
    otherwise -> None

{- top level functions -}
-- Takes a random seed at top level, then pass it down to each individual level
createInitialGameWithSeed : Random.Seed -> Game
createInitialGameWithSeed s = {
      level = ClickTheScreenToContinue
    , globalSeed = s
    }

-- This is borrowed from https://github.com/jcollard/random-examples/blob/master/src/TimeBasedDice.elm
-- The point is to take the page timestamp as the seed value for Random
start : Signal Time
start = (\ (t,_) -> t) <~ (timestamp <| constant ())

startTime : Signal (Maybe Time)
startTime = (\ (t,_) -> Just t) <~ sampleOn (fps 10) (timestamp <| constant ())

okayToSample : Int -> Bool
okayToSample fired = fired < 2

okay : Signal Bool
okay = okayToSample <~ (foldp (+) 0 (sampleOn startTime (constant 1)))

seed : Signal (Maybe Random.Seed)
seed = (\ t ->
            case t of
              Nothing -> Nothing
              Just t -> Just << Random.initialSeed <| round t) <~ keepWhen okay Nothing startTime
-- end of borrowing

-- We start with Nothing
gameState : Signal (Maybe Game)
gameState = foldp stepGame Nothing ((,) <~ input ~ seed)

-- starting with Nothing, once we have the random seed, we turn it into a game
stepGame : (Input, Maybe Random.Seed) -> Maybe Game -> Maybe Game
stepGame ({mouseIsDown} as input, maybeSeed) maybeGame =
    case maybeSeed of
        Nothing -> Nothing
        Just seed ->
            case maybeGame of
                Nothing -> Just <| createInitialGameWithSeed seed
                Just ({level, globalSeed} as game) ->
                    case level of
                        ClickTheScreenToContinue ->
                            let
                                initialToDoLevelStateWithSeed = {initialToDoLevelState | levelSeed <- globalSeed}
                                initialToDoLevel = ToDoLevel initialToDoLevelState
                            in
                                if mouseIsDown then
                                    Just { game | level <- initialToDoLevel
                                    } else Just game
                        otherwise ->
                            let level' = stepLevel input level
                            in Just { game | level <- level' }


{- Level specific types -}
type ArrowDirection = Left | Right | None

type alias ToDoLevelState = {
      score:Int
    , trials:Int
    , onScreenArrowDirection:ArrowDirection
    , levelSeed: Random.Seed
    }

-- Unfortunately elm doesn't seem to support adding the levelSeed dynamically when the level state is initialized.
-- So I put a placeholder seed 0 here.
initialToDoLevelState = {
                         score = 0
                       , trials = 0
                       , onScreenArrowDirection = Left
                       , levelSeed = Random.initialSeed 0
                       }

-- elm seems to lack constructor pattern matching
stepLevel : Input -> Level -> Level
stepLevel input level = case level of
    ToDoLevel ({score, trials, onScreenArrowDirection, levelSeed} as state) ->
        let
            (nextRandomDirection, seed') =
                Random.generate (Random.int 1 2) levelSeed |>
                    (\(randomInt, seed') -> (if randomInt % 2 == 0 then Left else Right, seed'))
            inputIsCorrect = if input.arrowKeyPressed == onScreenArrowDirection then True else False
            score' = score + (if inputIsCorrect then 1 else 0)
            trials' = trials + (if input.arrowKeyPressed == None then 0 else 1)
            onScreenArrowDirection' = if inputIsCorrect then nextRandomDirection else onScreenArrowDirection
        in ToDoLevel { state |
                       score <- score'
                     , trials <- trials'
                     , onScreenArrowDirection <- onScreenArrowDirection'
                     , levelSeed <- seed'
                     }

toModelTextElement f = Text.leftAligned << f << Text.monospace << Text.color Color.black << Text.fromString

-- display a game state
display : (Int,Int) -> Maybe Game -> Element
display (width, height) maybeGame =
    case maybeGame of
        Nothing ->
            container width height middle <|
                collage width height
                [ (toForm <| toModelTextElement (Text.height <| (toFloat height) / 20) <| "Loading")
                ]
        Just {level, globalSeed} ->
            case level of
                ClickTheScreenToContinue ->
                    container width height middle <|
                        collage width height
                        [ (toForm <| toModelTextElement (Text.height <| (toFloat height) / 20) <| "Click the screen to begin")
                        ]
                ToDoLevel state ->
                    let scores : Element
                        scores = toModelTextElement (Text.height 50) <|
                                 "Score: " ++ (toString state.score) ++ "/" ++ (toString state.trials)
                        arrowRotation = degrees (case state.onScreenArrowDirection of
                                                    Left -> 0
                                                    Right -> 180)
                    in
                    container width height middle <|
                        collage width height
                        [ (toForm <| toModelTextElement (Text.height <| (toFloat height) / 20) <| "Press the arrow key displayed") |> move (0, -(toFloat height)/2 + 50)
                         ,toForm scores |> move (0, (toFloat height)/2 - 40)
                         ,rotate arrowRotation (toForm <| toModelTextElement (Text.height <| (toFloat height) / 2) <| "←")
                        ]
main : Signal Element
main =  map2 display Window.dimensions gameState

