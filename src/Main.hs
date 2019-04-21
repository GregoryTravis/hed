{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import System.Exit
import System.IO

import Buffer
import Control
import Display
import Event
import SizeReport
import State
import Util

inputReader chan = forever $ do
  p <- getCharsOrSizeReport
  --msp ("parse", p)
  case p of Left dim -> writeChan chan (GotWindowSizeEvent dim)
            Right s -> mapM_ (\c -> writeChan chan (KeyEvent c)) s

transformEditorState :: EditorState -> Event -> EditorState
transformEditorState es (KeyEvent c) = es { buffer = Buffer c }
transformEditorState es (GotWindowSizeEvent dim) = es { screenDim = Just dim }

updateEditorState :: Chan Event -> Event -> ESAction ()
updateEditorState chan event = do
  s <- get
  let s' = transformEditorState s event
  put s'
  --io $ msp ("changey", s, s')
  io $ writeChan chan StateChangedEvent

eventLoop :: Chan Event -> ESAction a
eventLoop eventChan = forever $ do
  event <- io $ readChan eventChan
  --io $ msp ("Loop event", event)
  case event of ResizeEvent -> io updateTerminalSize
                QuitEvent -> io $ do msp "exiting"
                                     exitSuccess
                --GotWindowSizeEvent (w, h) -> io $ msp ("WSE", w, h)
                e@(GotWindowSizeEvent (w, h)) -> updateEditorState eventChan e
                KeyEvent 'q' -> io $ writeChan eventChan QuitEvent
                --KeyEvent c -> io $ msp ("key", c)
                KeyEvent c -> updateEditorState eventChan (KeyEvent c)
                StateChangedEvent -> redisplay

main :: IO ()
main = stateMain initEditorState $ do
  io $ do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    --msp "Hed start"

  eventChan <- io $ (newChan :: IO (Chan Event))
  --io $ do
  let wri = withRawInput 0 1
      wbt = withBackgroundThread (inputReader eventChan)
      wct = withWindowChangeHandler (writeChan eventChan ResizeEvent)
      loop = eventLoop eventChan
  --io $ writeChan eventChan StateChangedEvent
  io $ writeChan eventChan ResizeEvent
  (wri . wct . wbt) loop
