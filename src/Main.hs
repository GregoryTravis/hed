{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Chan
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
  
windowChangeHandler chan = do
  msp "windowChange handler"
  writeChan chan ResizeEvent

inputReader chan = do
  --c <- hGetChar stdin
  p <- getCharsOrSizeReport
  msp ("parse", p)
  case p of Left dim -> writeChan chan (GotWindowSizeEvent dim)
            Right s -> mapM_ (\c -> writeChan chan (KeyEvent c)) s
  inputReader chan

eventLoop eventChan = do
  let loop = do
        event <- readChan eventChan
        msp ("Loop event", event)
        case event of ResizeEvent -> updateTerminalSize
                      QuitEvent -> do 
                                      msp "exiting"
                                      exitSuccess
                      GotWindowSizeEvent (w, h) -> msp ("WSE", w, h)
                      KeyEvent 'q' -> writeChan eventChan QuitEvent
                      KeyEvent c -> msp ("key", c)
        loop
  loop

main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  msp "Hed start"

  eventChan <- newChan :: IO (Chan Event)
  let wri = withRawInput 0 1
      wbt = withBackgroundThread (inputReader eventChan)
      wst = withWindowChangeHandler (windowChangeHandler eventChan)
      loop = catchAndRestart (eventLoop eventChan) (writeChan eventChan QuitEvent)
  wri . wbt . wst $ loop
