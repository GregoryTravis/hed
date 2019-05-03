module Buffer
( Buffer(..)
, renderBuffer
, appendToBuffer
, getCursorRelativeOffset
) where

import Data.List.Split

import Types
import Util

renderBuffer :: Buffer -> (Int, Int) -> (Int, Int) -> [String]
renderBuffer buf (x, y) (w, h) = assertM "offset" ok hPaddedLines
  where contents = bufferContents buf
        lines = map (drop x) $ drop y $ take h $ splitOn "\n" contents
        pad line = (take w line) ++ take (w - (length line)) (repeat ' ')
        paddedLines = map pad lines
        hPaddedLines = paddedLines ++ (take (h - length paddedLines) $ repeat blankLine)
        blankLine = take w (repeat ' ')
        ok = x >=0 && y >= 0

appendToBuffer :: Buffer -> String -> Buffer
appendToBuffer buf s = buf { bufferContents = bufferContents buf ++ s }

getCursorRelativeOffset :: Buffer -> Int -> (Int, Int)
getCursorRelativeOffset (Buffer { bufferContents = s }) cursorPos = (x, y)
  where ss = splitOn "\n" (take cursorPos s)
        y = length ss - 1
        x = length (last ss)
