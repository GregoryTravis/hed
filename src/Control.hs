module Control
( withTerminalSetup
) where

import Control.Exception (finally, catch, bracket)
import Control.Monad.IO.Class
import Control.Monad.State
import System.IO
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
withWindowChangeHandler handler action = withSignalHandler windowChange handler action

-- Taken from https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input/36297897#36297897
withRawInput :: Int -> Int -> IO a -> IO a
withRawInput vmin vtime action = do

  {- retrieve current settings -}
  oldTermSettings <- getTerminalAttributes stdInput

  {- modify settings -}
  let newTermSettings = 
        flip withoutMode  EnableEcho   . -- don't echo keystrokes
        flip withoutMode  ProcessInput . -- turn on non-canonical mode
        flip withTime     vtime        . -- wait at most vtime decisecs per read
        flip withMinInput vmin         $ -- wait for >= vmin bytes per read
        oldTermSettings

  {- when we're done -}
  let revert = do setTerminalAttributes stdInput oldTermSettings Immediately
                  return ()

  {- install new settings -}
  setTerminalAttributes stdInput newTermSettings Immediately

  {- restore old settings no matter what; this prevents the terminal
   - from becoming borked if the action halts with an exception
   -}
  action `finally` revert

withStdoutBuffering :: BufferMode -> IO a -> IO a
withStdoutBuffering mode action = do
  oldMode <- hGetBuffering stdout
  hSetBuffering stdout mode
  a <- action
  hSetBuffering stdout oldMode
  return a

withTerminalSetup :: IO a -> IO a
withTerminalSetup action = do
  hSetBuffering stdin NoBuffering
  -- hSetBuffering stdout NoBuffering
  let wri = withRawInput 0 1
      wct = withWindowChangeHandler (msp "windowChange!")
  (wri . wct) action
