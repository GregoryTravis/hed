{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (finally, catch, IOException)
import Data.Char (ord)
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import qualified Data.Vector as V
import Control.Monad.State
import System.Console.ANSI
import qualified System.IO as IO
import System.Posix.IO (fdRead, stdInput)
import System.Posix.Terminal

import Util

box w h c = replicate h (pack (replicate w c))

drawBox (startX, startY) box = do
  mapM_ drawRow (zip [0..] box)
  where drawRow (y, row) = do setCursorPosition (startY + y) startX
                              putStr (unpack row)

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

data FileLines = FileLines (V.Vector Text) deriving Show

drawFileLines topLine (FileLines lines) = do
  mapM_ drawRow (zip [0..] (drop topLine (V.toList lines)))
  where drawRow (y, row) = do setCursorPosition y 0
                              putStr (unpack row)

readFileAsFL :: String -> IO FileLines
readFileAsFL filename = do
  entireFile <- IO.readFile filename
  let foo :: [Text]
      foo = T.splitOn "\n" (T.pack entireFile)
  return $ FileLines $ V.fromList $ T.splitOn "\n" (T.pack entireFile)

screen = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
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
  --speedTest
  fl <- readFileAsFL "sample.txt"
  msp "whey"
  msp fl
  let loop topLine = do
        c <- IO.hGetChar IO.stdin
        clearScreen
        setCursorPosition 0 0
        drawFileLines topLine fl
        let topLine' = case (ord c) of 107 -> topLine + 1
                                       106 -> topLine - 1
                                       _ -> topLine
        putStrLn (show (ord c))
        putStrLn (show topLine)
        loop topLine'
   in withRawInput 0 1 $ loop 0
  --threadDelay $ 2 * 1000000

push :: Int -> State [Int] ()
push x = state (\xs -> ((), x:xs))
pop :: State [Int] Int
pop = state (\(x:xs) -> (x, xs))
blah = do
  push 3
  a <- pop
  return a

main = msp $ runState blah [2]
