{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import System.Console.ANSI
import System.Exit
import System.IO

import Control
import Event
import SizeReport
import Util

inputReader chan = forever $ do
  p <- getCharsOrSizeReport
  --msp ("parse", p)
  case p of Left dim -> writeChan chan (GotWindowSizeEvent dim)
            Right s -> mapM_ (\c -> writeChan chan (KeyEvent c)) s

transformEditorState :: EditorState -> Event -> EditorState
transformEditorState es (KeyEvent c) = es { char = Just c, count = count es + 1 }

updateEditorState :: Chan Event -> Event -> ESAction ()
updateEditorState chan event = do
  s <- get
  let s' = transformEditorState s event
  put s'
  --io $ msp ("changey", s, s')
  io $ writeChan chan StateChangedEvent

redisplay :: ESAction ()
redisplay = do
  s <- get
  io $ do
    clearScreen
    setCursorPosition 0 0
    case char s of Nothing -> putStr "Hed 0.1"
                   Just c -> putStr $ take (count s) (repeat c)
    --msp ("redisplay", s)

eventLoop :: Chan Event -> ESAction a
eventLoop eventChan = forever $ do
  event <- io $ readChan eventChan
  --io $ msp ("Loop event", event)
  case event of ResizeEvent -> io updateTerminalSize
                QuitEvent -> io $ do msp "exiting"
                                     exitSuccess
                GotWindowSizeEvent (w, h) -> io $ msp ("WSE", w, h)
                KeyEvent 'q' -> io $ writeChan eventChan QuitEvent
                --KeyEvent c -> io $ msp ("key", c)
                KeyEvent c -> updateEditorState eventChan (KeyEvent c)
                StateChangedEvent -> redisplay

initState = EditorState { char = Nothing, count = 0 }

main :: IO ()
main = stateMain initState $ do
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
  io $ writeChan eventChan StateChangedEvent
  (wri . wct . wbt) loop
