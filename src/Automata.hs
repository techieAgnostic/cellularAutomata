{-# LANGUAGE DeriveGeneric #-}

module Automata where

import Comonad
import Spaces.Space1
import Spaces.Space2
import System.Random
import GHC.Generics
import Control.DeepSeq
import Data.Maybe

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

------------------------
-- grabbing neighbors --
------------------------

-- we want to be able to create a list of (Maybe CellState)
-- representing each neighbor, this way it will work on the
-- edges, and also we can fix the position of ecah neighbor
-- so that rules can be directional also.

grabNeighbors :: Space2 CellState -> [(Maybe CellState)]
grabNeighbors s = let
  tl = grabTopLeft s
  t  = grabTop s
  tr = grabTopRight s
  l  = grabLeft s
  r  = grabRight s
  bl = grabBotLeft s
  b  = grabBot s
  br = grabBotRight s
  in [tl, t, tr, l, r, bl, b, br]

grabTemplate :: (Space2 CellState -> Maybe (Space2 CellState))
  -> Space2 CellState -> Maybe CellState
grabTemplate f s = case f s of
  Nothing -> Nothing
  Just x -> Just $ extract x

grabTop, grabBot, grabLeft, grabRight :: Space2 CellState -> Maybe CellState
grabTop = grabTemplate up2
grabBot = grabTemplate down2
grabLeft = grabTemplate left2
grabRight = grabTemplate right2

maycom :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
maycom f g s = do
  x <- f s
  y <- g x
  return y

grabTopLeft, grabTopRight, grabBotLeft, grabBotRight :: Space2 CellState -> Maybe CellState
grabTopLeft = grabTemplate (maycom up2 left2)
grabTopRight = grabTemplate (maycom up2 right2)
grabBotLeft = grabTemplate (maycom down2 left2)
grabBotRight = grabTemplate (maycom down2 right2)

filtJust :: [(Maybe a)] -> [a]
filtJust [] = []
filtJust (Nothing:as) = filtJust as
filtJust ((Just a):as) = a:(filtJust as)

numMatch :: CellState -> [(Maybe CellState)] -> Int
numMatch c = length . (filter (== c)) . filtJust

--------------
-- 2d rules --
--------------

conway :: Space2 CellState -> CellState
conway s = case extract s of
  Rock -> Paper
  Paper -> if numSci == 3 then Scissors else Paper
  Scissors -> if numSci == 2 || numSci == 3 then Scissors else Paper
  where
    numSci = numMatch Scissors ns
    ns = grabNeighbors s

rps :: Space2 CellState -> CellState
rps s
  = case extract s of
      Rock -> if (numNs Paper) > 2 then Paper else Rock
      Paper -> if (numNs Scissors) > 2 then Scissors else Paper
      Scissors -> if (numNs Rock) > 2 then Rock else Scissors
  where
    numNs c = numMatch c $ grabNeighbors s
