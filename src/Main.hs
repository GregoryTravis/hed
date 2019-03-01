module Main where
import Control.Concurrent (threadDelay)

import System.Console.ANSI

-- Set colors and write some text in those colors.
main = do
  setSGR [SetColor Foreground Vivid Red]
  setSGR [SetColor Background Vivid Blue]
  clearScreen
  setCursorPosition 3 5
  putStrLn "Red-On-Blue"
  setSGR [Reset]  -- Reset to default colour scheme
  putStrLn "Default colors."
  threadDelay $ 2 * 1000000
