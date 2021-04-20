{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import System.Process
import System.Random
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import Data.Maybe (fromMaybe)
import Comonad
import Spaces.Space2
import Spaces.Space1
import Automata
import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent
import Control.DeepSeq

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

initGame :: IO (Space2 CellState)
initGame = do
  rng <- getStdGen
  return $ createRandSpace2 rng

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
    threadDelay 100000
  g <- initGame
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) (app h w) (clamp2 w h g)

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
