module Buffer
( Buffer(..)
, renderBuffer
, appendToBuffer
) where

import Data.List.Split

import Types
import Util

renderBuffer :: Buffer -> (Int, Int) -> (Int, Int) -> [String]
renderBuffer buf (ox, oy) (w, h) = assertM "offset" ok hPaddedLines
  where contents = bufferContents buf
        lines = map (drop ox) $ take h $ drop oy $ splitOn "\n" contents
        pad line = (take w line) ++ take (w - (length line)) (repeat ' ')
        paddedLines = map pad lines
        hPaddedLines = paddedLines ++ (take (h - length paddedLines) $ repeat blankLine)
        blankLine = take w (repeat ' ')
        ok = ox >=0 && oy >= 0

appendToBuffer :: Buffer -> String -> Buffer
appendToBuffer buf s = buf { bufferContents = bufferContents buf ++ s }
