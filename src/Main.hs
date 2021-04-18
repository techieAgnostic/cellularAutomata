module Main where

--import System.Random
import Control.Monad
import System.Process
import System.Random
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import Data.Maybe (fromMaybe)

-------------------
-- comonad class --
-------------------

class Functor w => Comonad w
  where
    (=>>)     :: w a -> (w a -> b) -> w b
    extract   :: w a -> a
    duplicate :: w a -> w (w a)
    x =>> f = fmap f (duplicate x)

------------
-- spaces --
------------

-- a locally focussed space
data Space t = Space [t] t [t]

-- spaces are also functors
instance Functor Space where
  fmap f (Space l c r) = Space (map f l) (f c) (map f r)

-- our space is a comonad
instance Comonad Space where
  -- duplicate will create a new space where
  -- the focussed element is our original space
  -- and each side is increasingly shifted copies
  -- in that direction
  duplicate w =
    Space (tail $ iterate left w)
          w
          (tail $ iterate right w)
  -- extract simply returns the focussed element
  extract (Space _ c _) = c

-- functions for moving the point
-- of locality.
-- todo: question the empty list cases
-- most spaces should be infinite
right :: Space t -> Space t
right s@(Space l c []) = s
right (Space l c (r:rs)) = Space (c:l) r rs

left :: Space t -> Space t
left s@(Space [] c r) = s
left (Space (l:ls) c r) = Space ls l (c:r)

-- bound will take an infinite space
-- and bound it by i and j on each side
-- (not including the focus) and
-- turn it into a list for printing
bound :: Int -> Int -> Space t -> [t]
bound i j (Space l c r) = (reverse (take i l)) ++ (c:(take j r))

-- boundw works as above, but the
-- entire list will be the size
-- given
boundw :: Int -> Space t -> [t]
boundw n = bound (x-m) x
  where
    o = if odd n then 1 else 0
    m = if even n then 1 else 0
    x = (n - o) `div` 2

-----------------------
-- cellular automata --
-----------------------

-- the states our cells can be in
-- may need to provide an ordering
-- may need to generalise the number
-- of states
data CellState = Alive | Dead
  deriving Eq

-- how the states are displayed on screen
-- this should probably be input to a function
-- rather than hardcoded
instance Show CellState
  where
    show Alive = "█"
    show Dead = " "

-- a rule stating how a cell is determined
rule :: Space CellState -> CellState
rule (Space (l:_) _ (r:_))
  | l == r = Dead
  | otherwise = Alive

-- a second rule for example
rule2 :: Space CellState -> CellState
rule2 (Space (l1:l2:_) m (r1:r2:_))
  | m == Alive && numAlive == 1 = Dead
  | m == Alive && numAlive == 4 = Dead
  | m == Dead && numAlive == 3 = Alive
  | otherwise = m
  where
    ns = [l1, l2, r1, r2]
    numAlive = length $ filter (== Alive) ns

rule3 :: Space CellState -> CellState
rule3 (Space (l:_) m (r:_))
  | l == r = m
  | otherwise = if m == Alive then Dead else Alive

-- take a space and a rule and
-- return the next space
step :: (Space t -> t) -> Space t -> Space t
step f w = w =>> f

---------------
-- rng stuff --
---------------

-- takes a generator and returns
-- an infinite list of bools
ilobs :: StdGen -> [Bool]
ilobs rng = b : (ilobs r)
  where
    (b,r) = random rng

-----------------
-- gross io bs --
-----------------

-- everything below this line deals with
-- input/output, and is therefore gross
-- i will clean this up one day, but it
-- hurts my soul.

------------------------
-- command line flags --
------------------------

-- structure containing the programs options
data Options = Options
  { optWidth       :: Int
  , optGenerations :: Int
  } deriving Show

-- the default options for the program
-- the width and generations are injected
-- and intended to be gotten at runtime
-- to match the window dimensions
defaultOptions :: Int -> Int -> Options
defaultOptions w h = Options
  { optWidth       = w
  , optGenerations = h
  }

-- the avaliable options
options :: [OptDescr (Options -> Options)]
options =
  [ Option ['w'] ["width"]
      (ReqArg (\w opts -> opts { optWidth = (read w) }) "WIDTH")
      "term width"
  , Option ['g'] ["generations"]
      (ReqArg (\t opts -> opts { optGenerations = (read t) }) "GENERATIONS")
      "time steps to simulate"
  ]

-- parse the options into the structure
-- erroring if encountering a flag not known to us
parseArgs :: IO Options
parseArgs = do
  argv <- getArgs
  progName <- getProgName
  tw <- readProcess "tput" [ "cols" ] ""
  th <- readProcess "tput" [ "lines" ] ""
  case getOpt RequireOrder options argv of
    (opts, [], []) -> return (foldl (flip id) (defaultOptions (read tw) (read th)) opts)
    (_, _, errs) -> ioError (userError (concat errs ++ helpMessage))
      where
        header = "Usage: " ++ progName ++ " [OPTION...]"
        helpMessage = usageInfo header options

---------------
-- main loop --
---------------

-- simply print the current space, then recurse to the next
runAutomata :: Space CellState -> Int -> Int -> IO ()
runAutomata s 0 w = putStrLn $ concat $ map show $ boundw w s
runAutomata s n w = do
  putStrLn $ concat $ map show $ boundw w s
  runAutomata (step rule s) (n - 1) w

main :: IO ()
main = do
  options <- parseArgs
  rng <- getStdGen
  let cs = map (\x -> if x then Alive else Dead) $ ilobs rng
  let w = (optWidth options)
  let h = (optGenerations options)
  let wh = (w + 1) `div` 2
  let m = head cs
  let l = take wh $ drop 1 cs
  let r = take wh $ drop wh $ drop 1 cs
  let s = Space (l ++ (repeat Dead)) m (r ++ (repeat Dead))
  -- non-random starting position for rule3 (the serpinski triangle)
  --let s = Space (repeat Dead) Alive (repeat Dead)
  runAutomata s h w
