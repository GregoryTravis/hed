module Main where

import Control.Concurrent (threadDelay)
import System.Console.ANSI
import System.IO

import Util

box w h c = replicate h (replicate w c)

drawBox (startX, startY) box = do
  mapM_ drawRow (zip [0..] box)
  where drawRow (y, row) = do
          mapM_ drawChar (zip [0..] row)
          where drawChar (x, c) = do
                  setCursorPosition (startY + y) (startX + x)
                  putStr [c]

fillScreen = do
  Just (h, w) <- getTerminalSize
  drawBox (0, 0) (box (w-0) (h-1) 'x')
  drawBox (0, (h-1)) (box (w-0) 1 'y')

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
  Just (h, w) <- getTerminalSize
  putStrLn (show (w, h))
  --drawBox (0, 0) (box w h 'x')
  let blast = mapM_ (\_ -> fillScreen) [0..9]
   in time "blast" blast
  --fillScreen
  let loop = do
        c <- hGetChar stdin
        --putStrLn [c]
        loop
   in loop
  --threadDelay $ 2 * 1000000
