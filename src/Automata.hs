{-# LANGUAGE DeriveGeneric #-}

module Automata where

import Comonad
import Spaces
import System.Random
import GHC.Generics
import Control.DeepSeq

-----------------------
-- cellular automata --
-----------------------

-- the states our cells can be in
-- may need to provide an ordering
-- may need to generalise the number
-- of states
data CellState = Rock | Paper | Scissors
  deriving (Eq, Bounded, Enum, Generic)

instance NFData CellState

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
    show Rock = "⬤"
    show Paper = " "
    show Scissors = "_"

-- -- a rule stating how a cell is determined
-- rule :: Space CellState -> CellState
-- rule (Space (l:_) _ (r:_))
--   | l == r = Dead
--   | otherwise = Alive
-- 
-- -- a second rule for example
-- rule2 :: Space CellState -> CellState
-- rule2 (Space (l1:l2:_) m (r1:r2:_))
--   | m == Alive && numAlive == 1 = Dead
--   | m == Alive && numAlive == 4 = Dead
--   | m == Dead && numAlive == 3 = Alive
--   | otherwise = m
--   where
--     ns = [l1, l2, r1, r2]
--     numAlive = length $ filter (== Alive) ns
-- 
-- rule3 :: Space CellState -> CellState
-- rule3 (Space (l:_) m (r:_))
--   | l == r = m
--   | otherwise = if m == Alive then Dead else Alive

--------------
-- 2d rules --
--------------

rps :: Space2 CellState -> CellState
rps (Space2 u m d)
  = case me of
      Rock -> if (length $ filter (== Paper) ns) > 2 then Paper else Rock
      Paper -> if (length $ filter (== Scissors) ns) > 2 then Scissors else Paper
      Scissors -> if (length $ filter (== Rock) ns) > 2 then Rock else Scissors
  where
    f b (Space (l:_) m (r:_)) = [l,r] ++ (if b then [m] else [])
    f b (Space [] m (r:_)) = [r] ++ (if b then [m] else [])
    f b (Space (l:_) m []) = [l] ++ (if b then [m] else [])
    f b (Space [] m []) = if b then [m] else []
    safeHead _ [] = []
    safeHead b (x:_) = f b x
    ns = concat [ (safeHead True u), (f False m), (safeHead True d) ]
    me = extract m

--conway :: Space2 CellState -> CellState
--conway (Space2 (u:_) m (d:_))
--  = case me of
--      Alive -> if (length ns) == 2 || (length ns == 3) then Alive else Dead
--      Dead -> if (length ns) == 3 then Alive else Dead
--  where
--    f b (Space (l:_) m (r:_)) = [l,r] ++ (if b then [m] else [])
--    ns = filter (== Alive) $ concat [ (f True u), (f False m), (f True d) ]
--    me = extract m
