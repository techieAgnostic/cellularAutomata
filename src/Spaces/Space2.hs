{-# LANGUAGE DeriveGeneric #-}

module Spaces.Space2 where

import Comonad
import Spaces.Space1

import System.Random
import Data.Maybe
import Control.DeepSeq
import GHC.Generics

-- a nested space
data Space2 t = Space2 [(Space t)] (Space t) [(Space t)]
  deriving (Generic, Generic1, Show)

-- generating strict data instances
instance NFData a => NFData (Space2 a)
instance NFData1 Space2

-- we can fmap into this structure by recursively fmapping
-- the inner spaces
instance Functor Space2 where
  fmap f (Space2 u m d) =
    Space2 (fmap (fmap f) u) (fmap f m) (fmap (fmap f) d)

-- map a partial function, converting to non maybe values
fintermap :: (a -> Maybe a) -> [a] -> [a]
fintermap _ [] = []
fintermap f (a:as) = case f a of
  Nothing -> []
  Just y -> y : fintermap f as

f :: ((Space2 a) -> Maybe (Space2 a)) -> Space (Space2 a) -> Maybe (Space (Space2 a))
f g (Space l m r) = case (g m) of
  Nothing -> Nothing
  Just y -> Just $ Space (fintermap g l) y (fintermap g r)

-- comonad instance for our 2d space
instance Comonad Space2 where
  -- to duplicate we must recursively duplicate in all directions
  -- the focussed space becomes the whole space, with left and right
  -- mapped to each side.
  -- to do the up and down lists, each needs to be the middle space
  -- mapped up and down as far as we can.
  -- up2 and down2 will return Nothing when they cant go further
  duplicate w =
    Space2 (finterate (f up2) dm) dm (finterate (f down2) dm)
      where
        dm = Space (finterate left2 w) w (finterate right2 w)
  -- to extract we simply recursively extract
  extract (Space2 _ m _) = extract m

-- directional moving of focus
up2 :: Space2 t -> Maybe (Space2 t)
up2 (Space2 [] _ _) = Nothing
up2 (Space2 (u:us) m d) = Just $ Space2 us u (m:d)

down2 :: Space2 t -> Maybe (Space2 t)
down2 (Space2 _ _ []) = Nothing
down2 (Space2 u m (d:ds)) = Just $ Space2 (m:u) d ds

noLeft :: Space t -> Bool
noLeft (Space [] _ _) = True
noLeft _ = False

noRight :: Space t -> Bool
noRight (Space _ _ []) = True
noRight _ = False

-- left and right require mapping further
-- we are assuming things are rectangular (maybe a bad idea?)
left2 :: Space2 t -> Maybe (Space2 t)
left2 (Space2 u m d) =
  if check
    then Nothing
    else Just $ Space2 (fmap (f . left) u) (f $ left m) (fmap (f . left) d)
  where
    check = noLeft m
    f l = fromJust l

right2 :: Space2 t -> Maybe (Space2 t)
right2 (Space2 u m d) =
  if check
    then Nothing
    else Just $ Space2 (fmap (f . right) u) (f $ right m) (fmap (f . right) d)
  where
    check = noRight m
    f l = fromJust l

-- clamp as we do in 1d Spaces
clampRel2 :: Int -> Int -> Int -> Int -> Space2 t -> Space2 t
clampRel2 w x y z (Space2 u m d) = Space2 (take w $ fmap f u) (f m) (take x $ fmap f d)
  where
    f = clampRel y z

clamp2 :: Int -> Int -> Space2 t -> Space2 t
clamp2 w h = clampRel2 nu nd nl nr
  where
    nu = h `div` 2
    nd = nu - (if even h then 1 else 0)
    nr = w `div` 2
    nl = nr - (if even w then 1 else 0)

mat2 :: Space2 t -> [[t]]
mat2 (Space2 u m d) = (reverse (fmap mat u)) ++ ((mat m):(fmap mat d))

matn2 :: Int -> Int -> Space2 t -> [[t]]
matn2 w h = mat2 . (clamp2 w h)

step :: Comonad w => (w t -> t) -> w t -> w t
step f w = w =>> f

-- create a randomly filled space
createRandSpace2 :: Random a => StdGen -> Space2 a
createRandSpace2 rng = 
  Space2 (tail $ map snd $ iterate f (r1, (createRandSpace r1)))
         (createRandSpace rng)
         (tail $ map snd $ iterate f (r2, (createRandSpace r2)))
  where
    f (r,s) = let (nr1,nr2) = split r in (nr2, (createRandSpace nr1))
    (r1,r2) = split rng
