module FrameBuffer
( FrameBuffer(..)
, ViewPos(..)
, debug
, getFrameBuffer
) where

import System.Console.ANSI

-- FrameBuffer (w, h)
data FrameBuffer = FrameBuffer (Int, Int) deriving Show

-- Position of the text character at the screen origin
data ViewPos = ViewPos Int Int deriving (Eq, Show)

getFrameBuffer = do
  Just (h, w) <- getTerminalSize
  return $ FrameBuffer (w, h)

debug fb@(FrameBuffer (w, h)) s = do
  setCursorPosition (h-1) 0
  putStr s
