{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Map as M
import System.Exit
import System.IO hiding (openFile)

import Buffer
import Control
import Display
import EditorState
import Event
import SizeReport
import State
import Util

inputReader chan = forever $ do
  p <- getCharsOrSizeReport
  --msp ("parse", p)
  case p of Left dim -> writeChan chan (GotWindowSizeEvent dim)
            Right s -> mapM_ (\c -> writeChan chan (KeyEvent c)) s

transformEditorState :: EditorState -> Event -> ESAction EditorState
transformEditorState es (KeyEvent ' ') = do
  return es { debugStr = show (getWindowUL es (currentWindowId es)) }
transformEditorState es (KeyEvent 'h') = do
  return $ moveCursor es (-1)
transformEditorState es (KeyEvent 'l') = do
  return $ moveCursor es 1
transformEditorState es (KeyEvent 'k') = do
  return $ moveCursorVertically es (-1)
transformEditorState es (KeyEvent 'j') = do
  return $ moveCursorVertically es 1
transformEditorState es (KeyEvent 'n') = return $ nextWindow es
transformEditorState es (KeyEvent c) = return $ es { buffers = updated, debugStr = "key " ++ [c] }
  where updated = buffers es
  --where updated = M.insert (currentBuffer es) (makeCharBuffer c) (buffers es)
transformEditorState es (GotWindowSizeEvent dim) = return $ es { screenDim = Just dim }

updateEditorState :: Chan Event -> Event -> ESAction ()
updateEditorState chan event = do
  s <- get
  s' <- transformEditorState s event
  put s'
  --io $ msp ("changey", s, s')
  io $ writeChan chan StateChangedEvent

shew :: String -> ESAction ()
shew s = do
  es <- get
  let scratch = buffers es M.! "scratch"
  put $ es { buffers = M.insert "scratch" (appendToBuffer scratch (s ++ "\n")) (buffers es) }

eventLoop :: Chan Event -> ESAction a
eventLoop eventChan = forever $ do
  event <- io $ readChan eventChan
  --io $ msp ("Loop event", event)
  case event of ResizeEvent -> io updateTerminalSize
                QuitEvent -> do es <- get
                                io $ do msp "exiting"
                                        clearScreen
                                        setCursorPos (0, 0)
                                        msp es
                                        exitSuccess
                --GotWindowSizeEvent (w, h) -> io $ msp ("WSE", w, h)
                e@(GotWindowSizeEvent (w, h)) -> updateEditorState eventChan e
                KeyEvent 'q' -> io $ writeChan eventChan QuitEvent
                --KeyEvent c -> io $ msp ("key", c)
                KeyEvent c -> updateEditorState eventChan (KeyEvent c)
                StateChangedEvent -> redisplay

main :: IO ()
main = stateMain initEditorState $ do
  openFile "uni.txt"
  openFile "inu.txt"
  newWindow "uni.txt"
  --switchToWindow 2
  esaction nextWindow
  --shew "ho"
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
