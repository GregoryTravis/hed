module Display
( redisplay
) where

import Control.Monad.State
import qualified Data.Map as M
import System.Console.ANSI
import System.IO

import Buffer
import Control
import Layout
import State
import Util

redisplay :: ESAction ()
redisplay = do
  s <- get
  let t = buffers s M.! currentBuffer s
  let other = buffers s M.! "hey"
  io $ do
    clearScreen
    setCursorPosition 0 0
    case screenDim s of Nothing -> msp "Can't determine screen dimensions"
                        Just dim -> withStdoutBuffering (BlockBuffering Nothing) $ do 
                                      setCursorPosition 0 0
                                      putStr $ mconcat $ checkSize (renderLayout (HStack (Buf t) (Buf other)) dim) dim
                                      --putStr $ mconcat $ checkSize (renderBuffer t dim) dim
                                      --putStrLn $ show $ length $ checkSize (renderBuffer t dim) dim
                                      --msp s
                                      setCursorPosition 0 0
                                      hFlush stdout
  --where checkSize s (w, h) = assertM "dim" (length s == w*h) s
  where checkSize :: [String] -> (Int, Int) -> [String]
        checkSize s (w, h) = assertM "dim" ok s
          where ok = (length s == h) && (all (w==) (map length s))
