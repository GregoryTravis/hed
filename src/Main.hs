{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception (finally, catch, bracket, AsyncException(..))
import qualified Data.Map as M
import System.Exit
import System.IO
import System.Posix.IO (fdRead, stdInput)
import System.Posix.Signals
import System.Posix.Signals.Exts
import System.Posix.Terminal

import SizeReport
import Util

-- Taken from https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input/36297897#36297897
withRawInput :: Int -> Int -> IO a -> IO a
withRawInput vmin vtime application = do

  {- retrieve current settings -}
  oldTermSettings <- getTerminalAttributes stdInput

  {- modify settings -}
  let newTermSettings = 
        flip withoutMode  EnableEcho   . -- don't echo keystrokes
        flip withoutMode  ProcessInput . -- turn on non-canonical mode
        flip withTime     vtime        . -- wait at most vtime decisecs per read
        flip withMinInput vmin         $ -- wait for >= vmin bytes per read
        oldTermSettings

  {- install new settings -}
  setTerminalAttributes stdInput newTermSettings Immediately

  {- restore old settings no matter what; this prevents the terminal
   - from becoming borked if the application halts with an exception
   -}
  application 
    `finally` do setTerminalAttributes stdInput oldTermSettings Immediately
                 return ()

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

withSignalHandler :: Signal -> Handler -> IO a -> IO a
withSignalHandler signal handler io = bracket install uninstall (\_ -> io)
  where install = installHandler signal handler Nothing
        uninstall originalHandler = do msp "uninstall"
                                       installHandler signal originalHandler Nothing

withBackgroundThread backgroundIO io = bracket (forkIO backgroundIO) killThread' (\_ -> io)
  where killThread' tid = do msp "killThread"
                             killThread tid

catchAndRestart io onerr = catch io catcher
  where catcher :: AsyncException -> IO ()
        catcher e = do
          onerr
          catch io catcher

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
      wst = withSignalHandler windowChange (Catch (windowChangeHandler eventChan))
      loop = catchAndRestart (eventLoop eventChan) (writeChan eventChan QuitEvent)
  wri . wbt . wst $ loop
