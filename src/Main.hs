module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (finally, catch, IOException)
import qualified Data.Text as T
import System.Console.ANSI
import System.IO
import System.Posix.IO (fdRead, stdInput)
import System.Posix.Terminal


import Util

box w h c = replicate h (T.pack (replicate w c))

drawBox (startX, startY) box = do
  mapM_ drawRow (zip [0..] box)
  where drawRow (y, row) = do setCursorPosition (startY + y) startX
                              putStr (T.unpack row)

fillScreen = do
  Just (h, w) <- getTerminalSize
  drawBox (0, 0) (box (w-0) (h-1) 'x')
  drawBox (0, (h-1)) (box (w-0) 1 'y')

speedTest = time "speedTest" $ mapM_ (\_ -> fillScreen) [0..99]

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
    `finally` setTerminalAttributes stdInput oldTermSettings Immediately

main = do
  hSetBuffering stdout NoBuffering
  setSGR [SetColor Foreground Vivid Red]
  setSGR [SetColor Background Vivid Blue]
  clearScreen
  setCursorPosition 3 5
  putStrLn "Red-On-Blue"
  setSGR [Reset]  -- Reset to default colour scheme
  putStrLn "Default colors."
  Just (h, w) <- getTerminalSize
  putStrLn (show (w, h))
  --fillScreen
  --drawBox (3, 3) (box 8 8 'r')
  setCursorPosition 0 0
  speedTest
  let loop = do
        c <- hGetChar stdin
        setCursorPosition 0 0
        putStr [c]
        loop
   in withRawInput 0 1 loop
  --threadDelay $ 2 * 1000000
