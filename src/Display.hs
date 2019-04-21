module Display
( redisplay
) where

import Control.Monad.State
import System.Console.ANSI

import State
import Thing
import Util

redisplay :: ESAction ()
redisplay = do
  s <- get
  let t = thing s
  io $ do
    --clearScreen
    setCursorPosition 0 0
    case screenDim s of Nothing -> msp "Can't determine screen dimensions"
                        Just dim -> do putStr $ renderThing t dim
                                     --msp s
