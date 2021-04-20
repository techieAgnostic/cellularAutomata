module Main where

import Comonad
import Automata
import BrickStuff
import Options
import Spaces.Space2

import Brick
import Brick.BChan
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent
import qualified Graphics.Vty as V

---------------
-- main loop --
---------------

main :: IO ()
main = do
  options <- parseArgs
  let w = (optWidth options)
  let h = (optHeight options)
  chan <- newBChan 1
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay $ (optTime options) * 10000
  g <- initGame
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) (app h w) (clamp2 w h g)

