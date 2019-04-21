module Display
( redisplay
) where

import Control.Monad.State
import System.Console.ANSI

import State
import Thing

redisplay :: ESAction ()
redisplay = do
  s <- get
  let t = thing s
  io $ do
    clearScreen
    setCursorPosition 0 0
    putStr $ renderThing t (10, 10)
