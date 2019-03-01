module Main where

import Control.Concurrent (threadDelay)
import System.IO

import System.Console.ANSI

-- Set colors and write some text in those colors.
main = do
  hSetBuffering stdout NoBuffering
  setSGR [SetColor Foreground Vivid Red]
  setSGR [SetColor Background Vivid Blue]
  clearScreen
  setCursorPosition 3 5
  putStrLn "Red-On-Blue"
  setSGR [Reset]  -- Reset to default colour scheme
  putStrLn "Default colors."
  let loop = do
        c <- hGetChar stdin
        putStrLn [c]
        loop
   in loop
  --threadDelay $ 2 * 1000000
