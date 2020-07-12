{-# LANGUAGE DeriveFunctor #-}

module Comonad19
  ( initGrid
  , step
  , display
  , simpleSimulationExample
  , simulate_
  , simulate
  , Config(..)
  , Grid(..)
  ) where

import Control.Comonad (Comonad, duplicate, extend, extract)
import Control.Monad.State (State, get, put, runState)
import System.Random (StdGen, newStdGen, next, random, split)

-- | Implementation of ListZipper and corresponding functions/instances.
data ListZipper a = LZ [a] a [a]

listLeft :: ListZipper a -> ListZipper a
listLeft  (LZ (a:as) x bs) = LZ as a (x:bs)
listLeft _                 = error "listLeft"

listRight :: ListZipper a -> ListZipper a
listRight (LZ as x (b:bs)) = LZ (x:as) b bs
listRight _                = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (a -> a) -> (a -> a) -> a -> ListZipper a
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

instance Functor ListZipper where
    fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  duplicate = genericMove listLeft listRight

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) } deriving Functor

up :: Grid a -> Grid a
up (Grid g) = Grid $ listLeft g

down :: Grid a -> Grid a
down (Grid g) = Grid $ listRight g

left :: Grid a -> Grid a
left (Grid g) = Grid $ fmap listLeft g

right :: Grid a -> Grid a
right (Grid g) = Grid $ fmap listRight g

gridRead :: Grid a -> a
gridRead (Grid z) = extract $ extract $ z

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right

vertical :: Grid a -> ListZipper (Grid a)
vertical = genericMove up down

instance Comonad Grid where
  extract = gridRead
  duplicate = Grid . fmap horizontal . vertical

neighbours :: [Grid a -> Grid a]
neighbours = horiz ++ vert
  where
    horiz = [left, right]
    vert  = [up, down]

-- | Status of the grid's cell.
-- | Susceptible -> _
-- | InfectedIncubation -> *
-- | InfectedSymptoms -> #
-- | Recovered -> @
data Status
  = Susceptible StdGen
  | InfectedIncubation StdGen Int
  | InfectedSymptoms StdGen Int
  | Recovered StdGen Int

-- | Data type that represents parameters that are passed to simulation.
data Config = Config
  { probability    :: Double
  , daysSymptoms   :: Int
  , daysIncubation :: Int
  , daysImmunity   :: Int
  }

statusRule :: Config -> Grid (Status) -> Status
statusRule config z = ruleForStatus (gridRead z) where
  ruleForStatus :: Status -> Status
  ruleForStatus (Recovered g days)
    | days == 1 = Susceptible g
    | otherwise = Recovered g (days - 1)
  ruleForStatus (InfectedIncubation g days)
    | days == 1 = InfectedSymptoms g (daysSymptoms config)
    | otherwise = InfectedIncubation g (days - 1)
  ruleForStatus (InfectedSymptoms g days)
    | days == 1 = Recovered g (daysImmunity config)
    | otherwise = InfectedSymptoms g (days - 1)
  ruleForStatus (Susceptible g) =
      let dangerousCount = length $ filter (\dir -> (isDangerous . extract . dir) z) neighbours
      in newStatusForSusceptible dangerousCount g
  newStatusForSusceptible :: Int -> StdGen -> Status
  newStatusForSusceptible 0 g = Susceptible g
  newStatusForSusceptible n g = do
    let (rands, g') = getRandomNTimes n g
    if any (< probability config) rands then
      InfectedIncubation g' (daysIncubation config)
    else
      Susceptible g'
  isDangerous :: Status -> Bool
  isDangerous (Susceptible _) = False
  isDangerous (Recovered _ _) = False
  isDangerous _               = True

getRandomNTimes :: Int -> StdGen -> ([Double], StdGen)
getRandomNTimes n g = getRandomNTimes' n g []
  where
    getRandomNTimes' :: Int -> StdGen -> [Double] -> ([Double], StdGen)
    getRandomNTimes' 0 g' acc = (acc, g')
    getRandomNTimes' i g' acc =
      let (res, g'') = runState getRandom g'
      in getRandomNTimes' (i - 1) g'' (res : acc)

getRandom :: State StdGen Double
getRandom = do
    gen <- get
    let (val, newGen) = random gen :: (Double, StdGen)
    put newGen
    return val

-- | Returns initial grid with one infected person in the middle.
initGrid :: Config -> StdGen -> Grid Status
initGrid (Config _ _ incDays _) gen =
  let empty = emptyGrid gen
  in gridWrite (InfectedIncubation gen incDays) empty

-- | Returns empty grid.
emptyGrid :: StdGen -> Grid Status
emptyGrid gen = Grid $ genericMove (nextZipper fst) (nextZipper snd) (createRow gen)
  where
    nextZipper :: ((StdGen, StdGen) -> StdGen) -> ListZipper Status -> ListZipper Status
    nextZipper f (LZ _ (Susceptible gen') _) = createRow $ f $ split $ nextGenerator gen'
    nextZipper _ _                           = error "no ill people on empty grid"
    nextElement :: ((StdGen, StdGen) -> StdGen) -> Status -> Status
    nextElement f (Susceptible gen') = Susceptible $ f $ split gen'
    nextElement _ _                  = error "no ill people on empty grid"
    nextGenerator :: StdGen -> StdGen
    nextGenerator gen' = snd $ next gen'
    createRow :: StdGen -> ListZipper Status
    createRow gen' = genericMove (nextElement fst) (nextElement snd) (Susceptible gen')

-- | Makes one step of simulation.
step :: Config -> Grid Status -> Grid Status
step config grid = extend (statusRule config) grid

-- | Displays `Grid` as a square with side of length radius * 2.
display :: Int -> Grid Status -> String
display radius grid = (unlines $ map displayLine $ toList $ unGrid grid) ++ "\n"
  where displayLine z' = map displayChar $ toList z'
        displayChar (Susceptible _)          = '_'
        displayChar (Recovered _ _)          = '@'
        displayChar (InfectedSymptoms _ _)   = '#'
        displayChar (InfectedIncubation _ _) = '*'
        toList (LZ ls x rs) = reverse (take radius ls) ++ [x] ++ take radius rs

-- | Prints simulation step by step specified number of times.
simulate_ :: Config -> StdGen -> Int -> IO ()
simulate_ config gen steps = do
  let evolution = iterate (step config) (initGrid config gen)
  mapM_ (putStr . (display 10)) (take steps evolution)

simulate :: Config -> StdGen -> Int -> Int -> [[String]]
simulate config gen steps squareSide = do
  let evolution = iterate (step config) (initGrid config gen)
  map (lines . (display squareSide)) (take steps evolution)

-- | Simple simulation example.
simpleSimulationExample :: IO ()
simpleSimulationExample = do
  let config = Config 0.35 2 3 4
  stdGen <- newStdGen
  let steps = 15
  simulate_ config stdGen steps
  putStrLn ""
