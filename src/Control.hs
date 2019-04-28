module Control
( io
, stateMain
, withSignalHandler
, withBackgroundThread
, withRawInput
, withWindowChangeHandler
, withStdoutBuffering
, EditorState(..)
, ESAction
) where

import Control.Concurrent
import Control.Exception (finally, catch, bracket, AsyncException(..))
import Control.Monad.IO.Class
import Control.Monad.State
import System.IO
import System.Posix.IO (stdInput)
import System.Posix.Signals
import System.Posix.Signals.Exts
import System.Posix.Terminal

import State
import Util

stateMain :: t -> StateT t IO () -> IO ()
stateMain initState main = runStateT main initState >> return ()

{-
finallyStateT :: StateT t IO a -> StateT t IO b -> StateT t IO a 
finallyStateT a b = do
  s <- get
  liftIO $ runStateT s a `finally` runStateT s
-}

withSignalHandler :: Signal -> IO () -> IO a -> IO a
withSignalHandler signal handlerIO io = bracket install uninstall (\_ -> io)
  where install = installHandler signal (Catch handlerIO) Nothing
        uninstall originalHandler = do msp "uninstall"
                                       installHandler signal originalHandler Nothing

transformAsIO :: ESAction a -> (IO a -> IO a) -> ESAction a
transformAsIO esAction iot = do
  s <- get
  io $ iot $ runStateT esAction s >>= (\(a, s) -> return a)

withWindowChangeHandler :: IO () -> ESAction a -> ESAction a
withWindowChangeHandler handler action = transformAsIO action wrap
  where wrap ioAction = withSignalHandler windowChange handler ioAction

withBackgroundThread :: IO () -> ESAction a -> ESAction a
withBackgroundThread backgroundIO action = transformAsIO action wrap
  where wrap ioAction = bracket (forkIO backgroundIO) killThread' (\_ -> ioAction)
        killThread' tid = do msp "killThread"
                             killThread tid

-- Taken from https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input/36297897#36297897
withRawInput :: Int -> Int -> ESAction a -> ESAction a
withRawInput vmin vtime application = do

  {- retrieve current settings -}
  oldTermSettings <- io $ getTerminalAttributes stdInput

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
  io $ setTerminalAttributes stdInput newTermSettings Immediately

  {- restore old settings no matter what; this prevents the terminal
   - from becoming borked if the application halts with an exception
   -}
  transformAsIO application $ \io -> io `finally` revert

withStdoutBuffering :: BufferMode -> IO a -> IO a
withStdoutBuffering mode action = do
  oldMode <- hGetBuffering stdout
  hSetBuffering stdout mode
  a <- action
  hSetBuffering stdout oldMode
  return a
