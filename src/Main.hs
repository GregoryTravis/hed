{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Char (ord)
import qualified Data.Map as M
import System.Exit
import System.IO hiding (openFile)

import Buffer
import Control
import Display
import EditorState
import Event
import Layout
import Process
import SizeReport
import State
import Types
import Util

inputReader chan = forever $ do
  p <- getCharsOrSizeReport
  --msp ("parse", p)
  case p of Left dim -> writeChan chan (GotWindowSizeEvent dim)
            Right s -> mapM_ (\c -> writeChan chan (KeyEvent c)) s

keyBindings :: M.Map Char KeyBinding
keyBindings = M.fromList
  [ ('y', TransformBinding nextWindow)
  , ('h', TransformBinding $ \es -> moveCursor es (-1) 0)
  , ('l', TransformBinding $ \es -> moveCursor es 1 0)
  , ('k', TransformBinding $ \es -> moveCursor es 0 (-1))
  , ('j', TransformBinding $ \es -> moveCursor es 0 1)
  , ('n', TransformBinding nextWindow)
  , ('z', TransformBinding $ \es -> moveCursor (insertChar es 'z') 1 0)
  , ('\o177', TransformBinding $ \es -> moveCursor (deleteChar es) (-1) 0)
  , (' ', TransformBinding $ \es ->  es { debugStr = show (getWindowUL es (currentWindowId es)) })
  , ('s', ActionBinding $ saveCurrentBuffer >> showDebug "saved")
  ]

showDebug :: String -> ESAction ()
showDebug s = asAction $ \es -> es { debugStr = s }

executeKey :: Char -> ESAction ()
executeKey c =
  case (M.lookup c keyBindings) of Just keyBinding -> executeKeyBinding keyBinding
                                   Nothing -> noSuchKey c

executeKeyBinding (TransformBinding transformer) = do
  s <- get
  put $ transformer s
executeKeyBinding (ActionBinding action) = action

noSuchKey :: Char -> ESAction ()
noSuchKey c = do
  s <- get
  put $ s { debugStr = "No such key: " ++ [c] }

handleEvent :: Event -> ESAction ()
handleEvent (KeyEvent c) = executeKey c
handleEvent (GotWindowSizeEvent dim) = asAction $ \es -> es { screenDim = Just dim }

updateEditorState :: Chan Event -> Event -> ESAction ()
updateEditorState chan event = do
  --s <- get
  --s' <- transformEditorState s event
  --put s'
  --io $ msp ("changey", s, s')
  handleEvent event
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

saveCurrentBuffer = do
  es <- get
  let (name, Buffer { bufferContents = contents }) = currentBufAndNAme es
  io $ saveFile name contents

main :: IO ()
main = stateMain initEditorState $ do
  openFile "uni.txt"
  openFile "inu.txt"
  openFile "gol.txt"
  --attachProcess "gol.txt" gol
  --newWindow "uni.txt"
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
