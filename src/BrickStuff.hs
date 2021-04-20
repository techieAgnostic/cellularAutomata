{-# LANGUAGE OverloadedStrings #-}

module BrickStuff where

import Automata
import Spaces.Space2

import System.Random
import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

-----------------
-- brick stuff --
-----------------

data Tick = Tick
type Name = ()

-- App definition

app :: Int -> Int -> App (Space2 CellState) Tick Name
app h w = App { appDraw = drawUI h w
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

-- Handling events

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (rockAttr, V.red `on` V.blue)
  , (scissorsAttr, V.green `on` V.red)
  , (paperAttr, V.blue `on` V.green)
  ]

handleEvent :: (Space2 CellState) -> BrickEvent Name Tick -> EventM Name (Next (Space2 CellState))
handleEvent g (AppEvent Tick) = continue $ step rps g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g _ = continue g

drawUI :: Int -> Int -> Space2 CellState -> [Widget Name]
drawUI h w g = [ C.center $ drawGrid h w g ]

drawGrid :: Int -> Int -> Space2 CellState -> Widget Name
drawGrid h w g = vBox rows
  where
    bw = mat2 g   
    rows = [ hBox $ cellsInRow r | r <- bw ]
    cellsInRow y = map drawCell y

drawCell :: CellState -> Widget Name
drawCell Paper = withAttr paperAttr $ str " "
drawCell Scissors = withAttr scissorsAttr $ str " "
drawCell Rock = withAttr rockAttr $ str " "

rockAttr, scissorsAttr, paperAttr :: AttrName
rockAttr = "rockAttr"
paperAttr = "paperAttr"
scissorsAttr = "scissorsAttr"

initGame :: IO (Space2 CellState)
initGame = do
  rng <- getStdGen
  return $ createRandSpace2 rng
