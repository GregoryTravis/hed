module Control
( io
, stateMain
, withSignalHandler
, withBackgroundThread
, withRawInput
, withWindowChangeHandler
, catchAndRestart
, EditorState(..)
, ESAction
) where

import Control.Concurrent
import Control.Exception (finally, catch, bracket, AsyncException(..))
import Control.Monad.IO.Class
import Control.Monad.State
import System.Posix.IO (stdInput)
import System.Posix.Signals
import System.Posix.Signals.Exts
import System.Posix.Terminal

import Util

newtype EditorState = EditorState { stack :: [Integer] }

type ESAction a = StateT EditorState IO a

io :: IO a -> StateT t IO a
io = liftIO

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

withWindowChangeHandler :: IO () -> ESAction a -> ESAction a
withWindowChangeHandler handler action = do
  s <- get
  io $ withSignalHandler windowChange handler (runStateT action s >>= (\(a, s) -> return a))

withBackgroundThread backgroundIO io = bracket (forkIO backgroundIO) killThread' (\_ -> io)
  where killThread' tid = do msp "killThread"
                             killThread tid

catchAndRestart io onerr = catch io catcher
  where catcher :: AsyncException -> IO ()
        catcher e = do
          onerr
          catch io catcher

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

  {- install new settings -}
  io $ setTerminalAttributes stdInput newTermSettings Immediately

  {- restore old settings no matter what; this prevents the terminal
   - from becoming borked if the application halts with an exception
   -}
  s <- get
  io $ (runStateT application s >>= (\(a, s) -> return a))
         `finally` do setTerminalAttributes stdInput oldTermSettings Immediately
                      return ()
