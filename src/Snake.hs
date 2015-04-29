{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Concurrent
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Ord
import           GHC.Conc
import           System.Console.ANSI
import           System.IO

-- | The main entry point.
--TODO Return Type
main :: IO ((), Snake)
main = do
    hSetBuffering stdin NoBuffering
    clearScreen
    putStrLn "Snake\n"
    width   <- askSetting "Board Width"  defaultBoardWidth
    height  <- askSetting "Board Height" defaultBoardHeight
    speed   <- askSetting "Game Speed (1-10)"   defaultBoardSpeed
    length  <- askSetting "Snake Length" defaultSnakeLength
    bounded <- return defaultBounded
    runSnakeGame
        snakeTheGame
        (defaultRange width       10 1000           )
        (defaultRange height      10 1000           )
        (defaultRange (speed - 1) 0  9              )
        (defaultRange length      1  (width `div` 6))
        bounded
    putStrLn "Done!"
    return ((), Snake [] R (0,0))
        where defaultRange n minVal maxVal = max minVal $ min maxVal n

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
    getDirection :: Direction,
    getCritter :: (Int, Int)
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
        snake     = Snake startingTail R (boardHeight settings `div` 2, boardWidth settings `div` 2)
        startingTail = zip (reverse [0..length]) $ repeat startingY
        startingY = boardHeight settings `div` 2
    in runStateT (runReaderT (runSnake game) settings) snake

snakeTheGame :: SnakeGame ()
snakeTheGame = do
    tick 20 R
    tick 10 U
    tick 5 L
    tick 2 U
    tick 3 R
    tick 10 D
        where
            tick n dir = do
                snake <- get
                put $ Snake (getTail snake) dir (getCritter snake)
                replicateM_ n $ sequence [drawBoard, waitForInput, updateSnake]

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
                paddedCoords = replicate minY [] ++ snakeCoords ++ replicate (height - maxY) []
            in concatMap (drawLine width) paddedCoords
        drawLine width snakeCoords =
            [boundaryChar] ++
            map ((\bool -> if bool then snakeChar else ' ') . (`elem` snakeCoords)) [0..width-1] ++
            [boundaryChar] ++
            "\n"
        boundaryChar = '*'
        snakeChar = 'o'

waitForInput :: SnakeGame ()
waitForInput = do
    speed <- asks gameSpeed
    snake <- get
    newSnake <- liftIO $ do
      key <- newEmptyMVar
      putMVar key Nothing
      thread <- forkIO $ getSingleChar key
      threadDelay (baseSpeed * (10 - speed) )
      killThread thread
      input <- takeMVar key
      case inputDirection input of
        Nothing -> return snake
        --TODO make this work: Just dir -> return $ snake {getDirection = dir}
        Just dir -> return $ Snake (getTail snake) dir (getCritter snake)
    put newSnake
      where baseSpeed = 20000

getSingleChar :: MVar (Maybe Char) -> IO ()
getSingleChar key = do
  inChar <- getChar
  modifyMVar_ key $ const $ return $ Just inChar

inputDirection :: Maybe Char -> Maybe Direction
inputDirection (Just c) = case c of
    'w' -> Just U
    'a' -> Just L
    's' -> Just D
    'd' -> Just R
    _ -> Nothing
inputDirection _ = Nothing

updateSnake :: SnakeGame ()
updateSnake = do
    snake <- get
    put $ moveSnake snake
    newHead <- gets $ head . getTail
    return ()

moveSnake :: Snake -> Snake
moveSnake oldSnake = Snake (newHead : newTail) (getDirection oldSnake) (getCritter oldSnake)
    where direction = getDirection oldSnake
          snakeTail = getTail oldSnake
          (x, y)    = head snakeTail
          newTail   = init snakeTail
          newHead   = case direction of
                         R -> (x+1, y)
                         L -> (x-1, y)
                         U -> (x, y-1)
                         D -> (x, y+1)

