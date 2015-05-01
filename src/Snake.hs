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
snakeTheGame = forever $ do
  drawBoard
  waitForInput
  updateSnake

snakeTestGame :: SnakeGame ()
snakeTestGame = do
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
    liftIO $ clearScreen >> putStrLn ""
    width  <- asks boardWidth
    height <- asks boardHeight
    snake  <- get
    liftIO $ let newBoard = drawHorizontalBoundary width ++ drawBody height width snake ++ drawHorizontalBoundary width
             in seq newBoard $ putStr newBoard
    return ()
    where
        drawHorizontalBoundary width = replicate (width + 2) boundaryChar ++ "\n"
        drawBody height width snake =
                -- snake Coordinates grouped by y-coordinate, sorted by ascending x-coordinate
            let groupedCoords = groupBy ((==) `on` snd) $ sortBy (compare `on` snd) $ getTail snake
                minY = snd . head . head $ groupedCoords
                maxY = snd . head . last $ groupedCoords
                coordsNoMiddleGaps = snd $ foldl' padCoords (minY ,[]) groupedCoords
                snakeCoords = map (sort . map fst) coordsNoMiddleGaps
                -- TODO arrow here?
                -- TODO right fold
                -- FIXME Snake disappears when movind horizontally and crossing vertical border (span top/bottom)
                padCoords (n, coords) xs@((_,y):_) = if n /= y then
                                                             (y+1, coords ++ replicate (y - n + 1) [])
                                                          else
                                                             (n+1, coords ++ [xs])

                -- padCoords (n, coords) _ = (n+1, coords ++ [[]])
                paddedCoords = replicate minY [] ++ snakeCoords ++ replicate (height - maxY - 1) []
            in concatMap (drawLine width) paddedCoords
        drawLine width snakeCoords critterX =
            let line = map getBoardChar [0..width-1]
                getBoardChar n = if n `elem` snakeCoords
                                 then snakeChar
                                 else if (-n) `elem snakeCoords
      (\bool -> if bool then snakeChar else ' ')
            in [boundaryChar] ++ line ++ [boundaryChar] ++ "\n"

        boundaryChar = '*'
        snakeChar = 'o'

waitForInput :: SnakeGame ()
waitForInput = do
    speed <- asks gameSpeed
    snake <- get
    newSnake <- liftIO $ do
      key <- newEmptyMVar
      putMVar key Nothing
      thread <- forkIO $ forever $ getSingleChar key
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
getSingleChar key = forever $ do
  inChar <- getChar
  modifyMVar_ key $ const $ return $ Just inChar

inputDirection :: Maybe Char -> Maybe Direction
inputDirection (Just c) = case c of
    'w' -> Just U
    'a' -> Just L
    's' -> Just D
    'd' -> Just R
    -- TODO Add support for arrow Keys
    _ -> Nothing
inputDirection _ = Nothing

-- TODO split this into a few fcts
updateSnake :: SnakeGame ()
updateSnake = do
    snake <- gets getTail
    direction <- gets getDirection
    critter <- gets getCritter
    xBound <- asks boardWidth
    yBound <- asks boardHeight
    let snakeHead@(x, y) = head snake
        ateCritter       = snakeHead == critter
        newTail          = if ateCritter
                           then snake
                           else init snake
        newHead          = case direction of
                      R -> ((x+1) `rem` xBound, y)
                      L -> ((x-1) `mod` xBound, y)
                      U -> (x, (y-1) `mod` yBound)
                      D -> (x, (y+1) `rem` yBound)
        newCritter = if ateCritter then
                        genNewCritter
                     else
                        critter
    -- TODO record update syntax
    put $ Snake (newHead : newTail) direction newCritter
    return ()

genNewCritter = (2,2)

--moveSnake :: Snake -> Snake
--moveSnake oldSnake = Snake (newHead : newTail) (getDirection oldSnake) (getCritter oldSnake)
--    where direction = getDirection oldSnake
--          snakeTail = getTail oldSnake
--          (x, y)    = head snakeTail
--          newTail   = init snakeTail
--          newHead   = case direction of
--                         R -> (x+1, y)
--                         L -> (x-1, y)
--                         U -> (x, y-1)
--                         D -> (x, y+1)

