module Display
( redisplay
) where

import Control.Monad.State
import System.Console.ANSI

import State

redisplay :: ESAction ()
redisplay = do
  s <- get
  io $ do
    clearScreen
    setCursorPosition 0 0
    case char s of Nothing -> putStr "Hed 0.1"
                   Just c -> putStr $ take (count s) (repeat c)
