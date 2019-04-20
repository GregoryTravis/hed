module Control
( withSignalHandler
, withBackgroundThread
, withRawInput
, withWindowChangeHandler
, catchAndRestart
) where

import Control.Concurrent
import Control.Exception (finally, catch, bracket, AsyncException(..))
import System.Posix.IO (stdInput)
import System.Posix.Signals
import System.Posix.Signals.Exts
import System.Posix.Terminal

import Util

withSignalHandler :: Signal -> IO () -> IO a -> IO a
withSignalHandler signal handlerIO io = bracket install uninstall (\_ -> io)
  where install = installHandler signal (Catch handlerIO) Nothing
        uninstall originalHandler = do msp "uninstall"
                                       installHandler signal originalHandler Nothing

withWindowChangeHandler :: IO () -> IO a -> IO a
withWindowChangeHandler wcIO io = withSignalHandler windowChange wcIO io

withBackgroundThread backgroundIO io = bracket (forkIO backgroundIO) killThread' (\_ -> io)
  where killThread' tid = do msp "killThread"
                             killThread tid

catchAndRestart io onerr = catch io catcher
  where catcher :: AsyncException -> IO ()
        catcher e = do
          onerr
          catch io catcher

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
