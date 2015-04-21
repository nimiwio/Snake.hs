{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Ord
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
defaultBoardHeight = 100
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
    drawBoard
    waitForInput

-- TODO make more efficient and clean up
drawBoard :: SnakeGame ()
drawBoard = do
    width  <- asks boardWidth
    height <- asks boardHeight
    snake  <- get
    drawHorizontalBoundary width
    drawBody height width snake
    drawHorizontalBoundary width
    return ()
    where
        drawHorizontalBoundary width = liftIO $ replicateM_ (width + 2) drawBoundaryChar >> putStrLn ""
        drawBody height width snake = do
            let snakeCoords = map (sort . map fst) $ groupBy ((==) `on` snd) $ sortBy (compare `on` snd) $ getTail snake
            mapM_ (drawLine width) snakeCoords
        drawLine width snakeCoords = do
            liftIO drawBoundaryChar
            liftIO $ mapM_ ((\bool -> if bool then drawSnakeChar else putStr " ") . (`elem` snakeCoords)) [0..width-1]
            liftIO $ drawBoundaryChar >> putStrLn ""
        drawBoundaryChar = putStr "*"
        drawSnakeChar = putStr "o"

waitForInput :: SnakeGame ()
waitForInput = return ()
