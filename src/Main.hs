{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ansi
import SpeedTest
import Util

main :: IO ()
main = withTerminalSetup main'

main' :: (Int, Int) -> IO ()
main' terminalSize = do
  speedTestMain terminalSize
