module Main where

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
  deriving (Eq, Bounded, Enum)

instance Random CellState where
    random g = case randomR (fromEnum (minBound :: CellState), fromEnum (maxBound :: CellState)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

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
step :: Comonad w => (w t -> t) -> w t -> w t
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

-- this is kinda gross but if it works it works
takeGive :: Int -> [a] -> ([a],[a])
takeGive n as = ( (take n as), (drop n as) )

--------------------------
-- 2d cellular automata --
--------------------------

data Space2 t =
  Space2 [(Space t)]
          (Space t)
         [(Space t)]

instance Functor Space2 where
  fmap f (Space2 u m d) =
    Space2 (fmap (fmap f) u) (fmap f m) (fmap (fmap f) d)

instance Comonad Space2 where
  duplicate w =
    Space2 (tail $ iterate (f up2) dm)
           dm
           (tail $ iterate (f down2) dm)
    where
      f g (Space l m r) = Space (fmap g l) (g m) (fmap g r)
      dm = Space (tail $ iterate left2 w) w (tail $ iterate right2 w)
  extract (Space2 _ m _) = extract m

down2 :: Space2 t -> Space2 t
down2 w@(Space2 u m []) = w
down2 (Space2 u m (d:ds)) = Space2 (m:u) d ds

up2 :: Space2 t -> Space2 t
up2 w@(Space2 [] m d) = w
up2 (Space2 (u:us) m d) = Space2 us u (m:d)

left2 :: Space2 t -> Space2 t
left2 (Space2 u m d) = Space2 (fmap left u) (left m) (fmap left d)

right2 :: Space2 t -> Space2 t
right2 (Space2 u m d) = Space2 (fmap right u) (right m) (fmap right d)

bound2 :: Int -> Int -> Int -> Int -> Space2 t -> [[t]]
bound2 u d l r (Space2 uw mw dw) = (reverse (take u (map (bound l r) uw))) ++ ((bound l r mw):(take d (map (bound l r) dw)))

bound2w :: Int -> Int -> Space2 t -> [[t]]
bound2w x y = bound2 (r-q) r (n-m) n
  where
    o = if odd x then 1 else 0
    m = if even x then 1 else 0
    n = (x - o) `div` 2
    p = if odd y then 1 else 0 
    q = if even y then 1 else 0
    r = (y - p) `div` 2

--------------
-- 2d rules --
--------------

conway :: Space2 CellState -> CellState
conway (Space2 (u:_) m (d:_))
  = case me of
      Alive -> if (length ns) == 2 || (length ns == 3) then Alive else Dead
      Dead -> if (length ns) == 3 then Alive else Dead
  where
    f b (Space (l:_) m (r:_)) = [l,r] ++ (if b then [m] else [])
    ns = filter (== Alive) $ concat [ (f True u), (f False m), (f True d) ]
    me = extract m

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
  , optHeight      :: Int
  } deriving Show

-- the default options for the program
-- the width and generations are injected
-- and intended to be gotten at runtime
-- to match the window dimensions
defaultOptions :: Int -> Int -> Options
defaultOptions w h = Options
  { optWidth       = w
  , optGenerations = 40
  , optHeight      = h
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
  , Option ['h'] ["height"]
      (ReqArg (\t opts -> opts { optHeight = (read t) }) "HEIGHT")
      "term height"
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

createRandSpace :: StdGen -> Space CellState
createRandSpace rng =
  Space (tail $ map snd $ iterate f (r1, Alive))
        (fst (random rng))
        (tail $ map snd $ iterate f (r2, Alive))
  where
    f (r,b) = let (nb,nr) = (random r) in (nr,nb)
    (r1,r2) = split rng

createRandSpace2 :: StdGen -> Space2 CellState
createRandSpace2 rng = 
  Space2 (tail $ map snd $ iterate f (r1, (createRandSpace r1)))
         (createRandSpace rng)
         (tail $ map snd $ iterate f (r2, (createRandSpace r2)))
  where
    f (r,s) = let (nr1,nr2) = split r in (nr2, (createRandSpace nr1))
    (r1,r2) = split rng

-- simply print the current space, then recurse to the next
--runAutomata :: Space2 CellState -> Int -> Int -> IO ()
--runAutomata s 0 w = putStrLn $ concat $ map show $ boundw w s
--runAutomata s n w = do
--  mapM_ putStrLn $ map show $ concat $ bound2w w s
--  runAutomata (step conway s) (n - 1) w

main :: IO ()
main = do
  options <- parseArgs
  rng <- getStdGen
  let cs = map (\x -> if x then Alive else Dead) $ ilobs rng
  let w = (optWidth options)
  let h = (optHeight options)
  let g = (optGenerations options)
  let s = createRandSpace2 rng
  mapM_ (f w h) (loop conway g s)
  where
    f w h s = do
      mapM_ putStrLn $ map (concat . (map show)) $ bound2w w h s
      putStrLn (take w (repeat '-'))
    loop f n s = take n $ iterate (step f) s
