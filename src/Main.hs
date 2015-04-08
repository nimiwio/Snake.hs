-- | Main entry point to the application.
module Main where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           System.Console.ANSI

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Snake"
    sequence_ $ replicate 5 $ putStrLn ""
    width   <- askSetting "Board Width"  defaultBoardWidth
    height  <- askSetting "Board Height" defaultBoardHeight
    speed   <- askSetting "Game Speed"   defaultBoardSpeed
    length  <- askSetting "Snake Length" defaultSnakeLength
    bounded <- return defaultBounded
    let settings = snakeSettings width height speed length bounded
    putStr "Yo"
    putStr "\rG"
    clearScreen
    putStrLn "clear"
    --runSnakeGame $ newGame settings

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
isInteger str = all isNumber str && (not $ null str)

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

newtype Snake = Snake { getTail :: [(Int,Int)] }

type SnakeGame = ReaderT SnakeSettings (StateT Snake IO)

newGame :: SnakeSettings -> SnakeGame ()
newGame settings =
          undefined -- settings (runStateT snake ())
    where snake     = Snake $ zip [length..0] $ repeat startingY
          length    = snakeSize settings
          startingY = boardHeight settings `div` 2

-- Default snake settings
defaultSnakeLength = 5

-- Default board settings
defaultBoardWidth  = 25
defaultBoardHeight = 25
defaultBoardSpeed  = 3
defaultBounded     = True

runSnakeGame :: SnakeGame () -> IO ()
runSnakeGame game = do
    drawBoard

drawBoard :: IO ()
drawBoard = undefined
