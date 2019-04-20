{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception (finally, catch, bracket, AsyncException(..))
import Data.Char (isDigit)
import qualified Data.Map as M
import System.Console.ANSI
import System.Exit
import System.IO
import System.Posix.IO (fdRead, stdInput)
import System.Posix.Signals
import System.Posix.Signals.Exts
import System.Posix.Terminal

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
  --j <- getTerminalSize
  --msp ("size", j)

updateTerminalSize eventChan = do
  saveCursor
  setCursorPosition 999 999
  reportCursorPosition

-- Returns an uninstaller
installHandlers chan = do
  origWindowChangeHandler <- installHandler windowChange (Catch (windowChangeHandler chan)) Nothing
  return $ installHandler windowChange origWindowChangeHandler Nothing

data ParseState = Esc | LSQB | FirstDigit | SecondDigit | Success | Fail
  deriving (Eq, Ord)
type Recognizer = Char -> ParseState
stateMachine :: M.Map ParseState Recognizer
stateMachine = M.fromList
  [ (Esc, \c -> if c == '\ESC' then LSQB else Fail)
  , (LSQB, \c -> if c == '[' then FirstDigit else Fail)
  , (FirstDigit, \c -> if c >= '0' && c <= '9' then FirstDigit else if c == ';' then SecondDigit else Fail)
  , (SecondDigit, \c -> if c >= '0' && c <= '9' then SecondDigit else if c == 'R' then Success else Fail)
  ]

recognizeSizeReport :: IO (Bool, String)
recognizeSizeReport = step Esc []
  where step :: ParseState -> String -> IO (Bool, String)
        step state sofar = do
          c <- getChar
          case (stateMachine M.! state) c of Success -> return (True, sofar ++ [c])
                                             Fail -> return (False, sofar ++ [c])
                                             next -> step next (sofar ++ [c])

parseSizeReport :: String -> (Int, Int)
parseSizeReport s = (read first, read second)
  where first = takeWhile isDigit $ drop 2 s
        second = takeWhile isDigit $ drop 1 $ dropWhile isDigit $ drop 2 s

inputReader chan = do
  --c <- hGetChar stdin
  p <- recognizeSizeReport
  msp ("parse", p)
  case p of (True, s) -> writeChan chan (GotWindowSizeEvent (parseSizeReport s))
            (False, s) -> mapM_ (\c -> writeChan chan (KeyEvent c)) s
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
          --msp "catcher"
          onerr
          catch io catcher

main1 = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  msp "Hed start"

  eventChan <- newChan :: IO (Chan Event)
  --withSignalHandler windowChange (Catch (windowChangeHandler eventChan)) $ withBackgroundThread (inputReader eventChan) $ do
  (withBackgroundThread (inputReader eventChan)) .
    (withSignalHandler windowChange (Catch (windowChangeHandler eventChan)))
    $ do
      let loop = do
            event <- readChan eventChan
            msp ("Loop event", event)
            case event of ResizeEvent -> updateTerminalSize eventChan
                          QuitEvent -> do 
                                          msp "exiting"
                                          exitSuccess
                          GotWindowSizeEvent (w, h) -> msp ("WSE", w, h)
                          KeyEvent 'q' -> writeChan eventChan QuitEvent
                          KeyEvent c -> msp ("key", c)
            loop
      catchAndRestart loop (writeChan eventChan QuitEvent)

main = withRawInput 0 1 main1
