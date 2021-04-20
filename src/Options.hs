module Options where

import System.Environment
import System.Console.GetOpt
import System.Process

------------------------
-- command line flags --
------------------------

-- structure containing the programs options
data Options = Options
  { optWidth       :: Int
  , optGenerations :: Int
  , optHeight      :: Int
  , optTime        :: Int
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
  , optTime        = 7
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
  , Option ['t'] ["time"]
      (ReqArg (\t opts -> opts { optTime = (read t) }) "TIME")
      "delay time"
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
