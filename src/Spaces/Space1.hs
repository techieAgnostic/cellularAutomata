{-# LANGUAGE DeriveGeneric #-}

module Spaces.Space1 where

import Comonad
import Control.DeepSeq
import GHC.Generics

-- a locally focussed space
data Space t = Space [t] t [t]
  deriving (Generic, Generic1, Show)

-- allowing strict evaluation of a space
instance NFData a => NFData (Space a)
instance NFData1 Space

-- spaces are also functors
instance Functor Space where
  fmap f (Space l c r) = Space (map f l) (f c) (map f r)

-- moving a space focus right
right :: Space t -> Maybe (Space t)
right (Space _ _ []) = Nothing
right (Space l c (r:rs)) = Just $ Space (c:l) r rs

-- moving a space's focus left
left :: Space t -> Maybe (Space t)
left (Space [] _ _) = Nothing
left (Space (l:ls) c r) = Just $ Space ls l (c:r)

-- iterate until we reach an edge
finterate :: (a -> Maybe a) -> a -> [a]
finterate f x = case (f x) of
  Nothing -> []
  Just y -> y : finterate f y

-- our space is a comonad
instance Comonad Space where
  -- duplicate creats a meta space
  duplicate w =
    Space (finterate left w)
          w
          (finterate right w)
  -- extract simply returns the focussed element
  extract (Space _ c _) = c

-- clamp an infinite space to a finite space
-- relative to center
clampRel :: Int -> Int -> Space t -> Space t
clampRel x y (Space l m r) = Space (take x l) m (take y r)

-- as above, but with a set width
-- if the width is even, we need to take one less from the left
clamp :: Int -> Space t -> Space t
clamp w (Space l m r) = Space (take ln l) m (take h r)
  where
    h = w `div` 2
    ln = h - (if even w then 1 else 0)

-- materialises a space, will hang if infinite
mat :: Space t -> [t]
mat (Space l m r) = (reverse l) ++ (m:r)

-- as above, but clamps to a given size first
matn :: Int -> Space t -> [t]
matn n = mat . (clamp n)

