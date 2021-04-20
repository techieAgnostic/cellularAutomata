{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Spaces where

import Comonad
import System.Random
import Control.DeepSeq
import GHC.Generics

------------
-- spaces --
------------

-- a locally focussed space
data Space t = Space [t] t [t]
  deriving (Generic, Generic1)

instance NFData a => NFData (Space a)
instance NFData1 Space

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
right w@(Space l m []) = w
right (Space l c (r:rs)) = Space (c:l) r rs

left :: Space t -> Space t
left w@(Space [] m r) = w
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

---------------
-- 2d spaces --
---------------

data Space2 t =
  Space2 [(Space t)]
          (Space t)
         [(Space t)]
  deriving (Generic, Generic1)

instance NFData a => NFData (Space2 a)
instance NFData1 Space2

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

bound2cw :: NFData t => Int -> Int -> Space2 t -> [[t]]
bound2cw x y w = bound2 (r-q) r (n-m) n $ clamp2 (r-q+1) (r+1) (n-m+1) (n+1) w
  where
    o = if odd x then 1 else 0
    m = if even x then 1 else 0
    n = (x - o) `div` 2
    p = if odd y then 1 else 0 
    q = if even y then 1 else 0
    r = (y - p) `div` 2

clamp2cw :: NFData t => Int -> Int -> Space2 t -> Space2 t
clamp2cw x y w = clamp2 (r-q+1) (r+1) (n-m+1) (n+1) w
  where
    o = if odd x then 1 else 0
    m = if even x then 1 else 0
    n = (x - o) `div` 2
    p = if odd y then 1 else 0 
    q = if even y then 1 else 0
    r = (y - p) `div` 2

clamp2 :: NFData t => Int -> Int -> Int -> Int -> Space2 t -> Space2 t
clamp2 u d l r (Space2 uw mw dw)
  = force $ Space2 (take u $ fmap (clamp l r) uw)
           (clamp l r mw)
           (take d $ fmap (clamp l r) dw)

clamp :: NFData t => Int -> Int -> Space t -> Space t
clamp x y (Space l m r) = force $ Space (take x l) m (take y r)

-- take a space and a rule and
-- return the next space
step :: Comonad w => (w t -> t) -> w t -> w t
step f w = w =>> f

-------------------
-- Random Spaces --
-------------------

createRandSpace :: Random a => StdGen -> Space a
createRandSpace rng =
  Space (tail $ map snd $ iterate f (r1, (fst (random rng))))
        (fst (random rng))
        (tail $ map snd $ iterate f (r2, (fst (random rng))))
  where
    f (r,b) = let (nb,nr) = (random r) in (nr,nb)
    (r1,r2) = split rng

createRandSpace2 :: Random a => StdGen -> Space2 a
createRandSpace2 rng = 
  Space2 (tail $ map snd $ iterate f (r1, (createRandSpace r1)))
         (createRandSpace rng)
         (tail $ map snd $ iterate f (r2, (createRandSpace r2)))
  where
    f (r,s) = let (nr1,nr2) = split r in (nr2, (createRandSpace nr1))
    (r1,r2) = split rng
