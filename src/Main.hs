{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Concurrent.Chan
-- import Control.Monad
-- import Control.Monad.IO.Class
-- import Control.Monad.State
-- import Data.Char (ord)
-- import qualified Data.Map as M
-- import System.Exit
-- import System.IO hiding (openFile)
import System.Console.ANSI

import Control
import Util

main :: IO ()
main = withTerminalSetup main'

main' :: IO ()
main' = do
  setCursorPosition 0 0
  msp "hey"
  sleep 100
