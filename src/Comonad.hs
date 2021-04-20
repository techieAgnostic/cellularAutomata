module Comonad where

-------------------
-- comonad class --
-------------------

class Functor w => Comonad w
  where
    (=>>)     :: w a -> (w a -> b) -> w b
    extract   :: w a -> a
    duplicate :: w a -> w (w a)
    x =>> f = fmap f (duplicate x)
