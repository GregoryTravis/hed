{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import System.Exit
import System.IO

import Control
import SizeReport
import Util

data Event =
  KeyEvent Char |
  ResizeEvent |
  GotWindowSizeEvent (Int, Int) |
  RedisplayEvent Char |
  QuitEvent
  deriving (Show)
  
inputReader chan = forever $ do
  p <- getCharsOrSizeReport
  msp ("parse", p)
  case p of Left dim -> writeChan chan (GotWindowSizeEvent dim)
            Right s -> mapM_ (\c -> writeChan chan (KeyEvent c)) s

eventLoop eventChan = forever $ do
  event <- readChan eventChan
  msp ("Loop event", event)
  case event of ResizeEvent -> updateTerminalSize
                QuitEvent -> do 
                                msp "exiting"
                                exitSuccess
                GotWindowSizeEvent (w, h) -> msp ("WSE", w, h)
                KeyEvent 'q' -> writeChan eventChan QuitEvent
                KeyEvent c -> msp ("key", c)

pop :: ESAction Integer
pop = do
  EditorState { stack = (x:xs) } <- get
  put EditorState { stack = (xs) }
  return x

push :: Integer -> ESAction ()
push x = do
  EditorState { stack = (xs) } <- get
  put EditorState { stack = (x:xs) }
  return ()

initState = EditorState { stack = [] }

main :: IO ()
main = stateMain initState $ do
  () <- push 10
  () <- push 20
  x <- pop
  io $ print x
  y <- pop
  io $ print y
  io $ do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    msp "Hed start"

  eventChan <- io $ (newChan :: IO (Chan Event))
  --io $ do
  let wri = withRawInput 0 1
      wbt = withBackgroundThread (inputReader eventChan)
      wct = withWindowChangeHandler (writeChan eventChan ResizeEvent)
      loop = catchAndRestart (eventLoop eventChan) (writeChan eventChan QuitEvent)
  (wri . wct) (io $ wbt $ loop)
