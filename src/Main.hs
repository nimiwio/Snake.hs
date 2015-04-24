{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Ord
import           GHC.Conc
import           System.Console.ANSI

-- | The main entry point.
--TODO Return Type
main :: IO ((), Snake)
main = do
    clearScreen
    putStrLn "Snake\n"
    width   <- askSetting "Board Width"  defaultBoardWidth
    height  <- askSetting "Board Height" defaultBoardHeight
    speed   <- askSetting "Game Speed"   defaultBoardSpeed
    length  <- askSetting "Snake Length" defaultSnakeLength
    bounded <- return defaultBounded
    runSnakeGame snakeTheGame width height speed length bounded
    putStrLn "Done!"
    return ((), Snake [] R)

askSetting :: String -> Int -> IO Int
askSetting settingName defaultVal = do
    putStrLn $ "Please enter setting: " ++ settingName
    input <- getLine
    if isInteger input then
        return $ read input
    else case input of
        [] -> do
            putStrLn "Using default setting."
            return defaultVal
        _  -> do
            putStrLn $ "\"" ++ input ++ "\" is not a valid number. Please enter a numeric value."
            askSetting settingName defaultVal

isInteger :: String -> Bool
isInteger str = all isNumber str && not (null str)

data SnakeSettings = SnakeSettings {
    boardWidth  :: Int,
    boardHeight :: Int,
    gameSpeed   :: Int,
    snakeSize   :: Int,
    isBounded   :: Bool
} deriving (Show)

snakeSettings :: Int -> Int -> Int -> Int -> Bool -> SnakeSettings
snakeSettings width height speed length bounded =
    SnakeSettings {
        boardWidth  = width,
        boardHeight = height,
        gameSpeed   = speed,
        snakeSize   = length,
        isBounded   = bounded
    }

data Snake = Snake {
    getTail :: [(Int,Int)],
    getDirection :: Direction
}

data Direction = L | R | U | D

newtype SnakeGame a = SnakeGame {
        runSnake :: ReaderT SnakeSettings (StateT Snake IO) a
    } deriving (Monad, MonadIO, MonadReader SnakeSettings, MonadState Snake)

newGame :: SnakeSettings -> SnakeGame ()
newGame settings =
          undefined -- settings (runStateT snake ())

-- Default snake settings
defaultSnakeLength = 6

-- Default board settings
defaultBoardWidth  = 100
defaultBoardHeight = 50
defaultBoardSpeed  = 5
defaultBounded     = True

runSnakeGame :: SnakeGame () -> Int -> Int -> Int -> Int -> Bool -> IO ((), Snake)
runSnakeGame game width height speed length bounded =
    let settings  = snakeSettings width height speed length bounded
        snake     = Snake startingTail R
        startingTail = zip (reverse [0..length]) $ repeat startingY
        startingY = boardHeight settings `div` 2
    in runStateT (runReaderT (runSnake game) settings) snake

snakeTheGame :: SnakeGame ()
snakeTheGame = do
    liftIO $ putStrLn "Here we go!"
    tick 20
    snake <- get
    put $ Snake (getTail snake) U
    tick 50
    return ()
        where tick n = replicateM_ n $ sequence [drawBoard, waitForInput, updateSnake]
    

-- TODO make more efficient and clean up
drawBoard :: SnakeGame ()
drawBoard = do
    liftIO clearScreen
    width  <- asks boardWidth
    height <- asks boardHeight
    snake  <- get
    liftIO $ let newBoard = drawHorizontalBoundary width ++ drawBody height width snake ++ drawHorizontalBoundary width
             in seq newBoard $ putStr newBoard
    return ()
    where
        drawHorizontalBoundary width = replicate (width + 2) boundaryChar ++ "\n"
        drawBody height width snake =
            let snakeCoords = map (sort . map fst) $ groupBy ((==) `on` snd) $ sortBy (compare `on` snd) $ getTail snake
                minY = minimum $ map snd $ getTail snake
                maxY = maximum $ map snd $ getTail snake
                paddedCoords = replicate minY [] ++ snakeCoords ++ replicate maxY []
            in concatMap (drawLine width) paddedCoords
        drawLine width snakeCoords =
            [boundaryChar] ++
            map ((\bool -> if bool then snakeChar else ' ') . (`elem` snakeCoords)) [0..width-1] ++
            [boundaryChar] ++
            "\n"
        boundaryChar = '*'
        snakeChar = 'o'

waitForInput :: SnakeGame ()
waitForInput = liftIO $ threadDelay 100000

updateSnake :: SnakeGame ()
updateSnake = do
    snake <- get
    put $ moveSnake snake

moveSnake :: Snake -> Snake
moveSnake oldSnake = Snake (newHead : newTail) $ getDirection oldSnake
    where direction = getDirection oldSnake
          snakeTail = getTail oldSnake
          (x, y)    = head snakeTail
          newTail   = init snakeTail
          newHead   = case direction of
                         R -> (x+1, y)
                         L -> (x-1, y)
                         U -> (x, y+1)
                         D -> (x, y-1)
