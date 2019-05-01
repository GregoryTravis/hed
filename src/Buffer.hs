module Buffer
( Buffer(..)
, renderBuffer
) where

--import Data.List (maximum)
import Data.List.Split

import Types

renderBuffer :: Buffer -> (Int, Int) -> [String]
renderBuffer buf (w, h) = hPaddedLines
  where contents = bufferContents buf
        lines = take h $ splitOn "\n" contents
        pad line = (take w line) ++ take (w - (length line)) (repeat ' ')
        paddedLines = map pad lines
        hPaddedLines = paddedLines ++ (take (h - length paddedLines) $ repeat blankLine)
        blankLine = take w (repeat ' ')
