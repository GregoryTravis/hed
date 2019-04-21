module Display
( redisplay
) where

import Control.Monad.State
import System.Console.ANSI
import System.IO

import Control
import State
import Thing
import Util

redisplay :: ESAction ()
redisplay = do
  s <- get
  let t = thing s
  io $ do
    clearScreen
    setCursorPosition 0 0
    case screenDim s of Nothing -> msp "Can't determine screen dimensions"
                        Just dim -> withStdoutBuffering (BlockBuffering Nothing) $ do 
                                      setCursorPosition 0 0
                                      putStr $ checkSize (renderThing t dim) dim
                                      --putStrLn $ show $ length $ checkSize (renderThing t dim) dim
                                      --msp s
                                      setCursorPosition 0 0
                                      hFlush stdout
  where checkSize s (w, h) = assertM "dim" (length s == w*h) s
