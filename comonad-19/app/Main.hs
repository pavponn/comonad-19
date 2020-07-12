module Main where

import System.Console.ANSI (SGR (..), clearScreen, cursorDown, setCursorPosition, setSGR, setTitle)
import System.IO (hFlush, stdout)
import System.Random (StdGen, newStdGen)

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Comonad19 (Config (..), simulate)

main :: IO ()
main = do
  stdGen <- newStdGen
  let config = Config
                 { probability    = 0.42
                 , daysSymptoms   = 7
                 , daysIncubation = 7
                 , daysImmunity   = 8
                 }
  resetScreen
  simulation config stdGen 100 20

resetScreen :: IO ()
resetScreen = clearScreen >> setSGR [Reset] >> setCursorPosition 0 0

pause :: IO ()
pause = do
    hFlush stdout
    threadDelay 250000

type Steps  = Int
type Radius = Int

simulation :: Config -> StdGen -> Steps -> Radius -> IO ()
simulation config gen steps radius = do
  setTitle "🦠Comonad-19"
  let grids = simulate config gen steps radius
  forM_ grids $ \grid -> do
    displayGrid grid
    pause
    setCursorPosition 0 0
  cursorDown (2 * radius + 1)

displayGrid :: [String] -> IO ()
displayGrid grid = do
  forM_ grid $ \lineOfGrid -> do
    displayLineOfGrid lineOfGrid

displayLineOfGrid :: String -> IO ()
displayLineOfGrid gridLine  = do
  forM_ gridLine $ \symbol -> do
    case symbol of
      '@' -> putStr "🟩"
      '#' -> putStr "🟥"
      '*' -> putStr "🟧"
      _   -> putStr "⬜"
  setSGR [Reset]
  putStrLn ""
