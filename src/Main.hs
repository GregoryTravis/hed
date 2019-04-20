{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception (finally, catch, IOException, AsyncException(..))
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Builder as B
import Data.Char (chr, ord, isDigit)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Vector (Vector, (!))
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Linear
import System.Console.ANSI
import System.Exit
import System.IO
import System.Posix.IO (fdRead, stdInput)
import System.Posix.Signals
import System.Posix.Signals.Exts
import System.Posix.Terminal

import Document
import FrameBuffer
import RectSampler
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
                 msp "ASDFASDF"
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

{-
sendFakeResizeEvent chan = do
  msp "send fake"
  writeChan chan ResizeEvent
-}

updateTerminalSize eventChan = do
  --msp "updateTerminalSize"
  saveCursor
  setCursorPosition 999 999
  reportCursorPosition

installHandlers chan = do
  origWindowChangeHandler <- installHandler windowChange (Catch (windowChangeHandler chan)) Nothing
  return origWindowChangeHandler

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

quit :: Handler -> IO ()
quit origWindowChangeHandler = do
  msp "exiting"
  installHandler windowChange origWindowChangeHandler Nothing
  exitSuccess



vvhandler :: MVar Int -> IO ()
vvhandler s_interrupted = do putStrLn "HI"
                             modifyMVar_ s_interrupted (return . (+1))

main1 = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  msp "Hed start"

  eventChan <- newChan :: IO (Chan Event)
  origWindowChangeHandler <- installHandlers eventChan
  otherThreadId <- forkIO $ inputReader eventChan
  let loop = do
        event <- readChan eventChan
        msp ("Loop event", event)
        case event of ResizeEvent -> updateTerminalSize eventChan
                      QuitEvent -> do 
                                      killThread otherThreadId
                                      quit origWindowChangeHandler
                      GotWindowSizeEvent (w, h) -> msp ("WSE", w, h)
                      KeyEvent 'q' -> writeChan eventChan QuitEvent
                      KeyEvent c -> msp ("key", c)
        loop
  let catcher :: AsyncException -> IO ()
      catcher e = do
        --msp "catcher"
        writeChan eventChan QuitEvent
        catch loop catcher
  catch loop catcher

main = withRawInput 0 1 main1
