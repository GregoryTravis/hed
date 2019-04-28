module Display
( redisplay
) where

import Control.Monad.State
import Data.List (length)
import qualified Data.Map as M
import System.Console.ANSI
import System.IO

import Buffer
import Control
import Layout
import State
import Util

redisplay :: ESAction ()
redisplay = do
  s <- get
  io $ do
    --clearScreen
    setCursorPosition 0 0
    case screenDim s of Nothing -> msp "Can't determine screen dimensions"
                        Just dim -> withStdoutBuffering (BlockBuffering Nothing) $ do 
                                      setCursorPosition 0 0
                                      clearScreen
                                      msp $ map length (renderLayout s (layout s) dim)
                                      putStr $ mconcat $ checkSize (renderLayout s (layout s) dim) dim
                                      --msp s
                                      setCursorPosition 0 0
                                      hFlush stdout
  where checkSize :: [String] -> (Int, Int) -> [String]
        checkSize s (w, h) = assertM "bad buffer rendering" ok s
          where ok = (length s == h) && (all (w==) (map length s))
