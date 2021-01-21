module SpeedTest (speedTestMain) where

import Prelude hiding (putStr)

import Control.Monad (zipWithM_)
import Data.Text hiding (take)
import Data.Text.IO (putStr)
import System.Console.ANSI

import Util

speedTestMain :: (Int, Int) -> IO ()
speedTestMain dim = do
  clearScreen
  fps <- perSec 1000 (drawRect dim)
  msp "\n"
  msp fps
  msp dim

drawRect :: (Int, Int) -> IO ()
drawRect (w, h) = do
  -- let ulx = w `div` 4
  --     uly = h `div` 4
  --     tw = w `div` 2
  --     th = h `div` 2
  let ulx = 0
      uly = 0
      tw = w
      th = h
      texts :: [Text]
      texts = take th (repeat line)
      line :: Text
      line = pack $ take tw (repeat 'q')
      draw x y s = do
        at x y
        putStr s
  zipWithM_ (draw ulx) [uly..uly+th-1] texts

at :: Int -> Int -> IO ()
at x y = setCursorPosition y x
