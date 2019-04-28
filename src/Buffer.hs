module Buffer
( Buffer(..)
, renderBuffer
, makeCharBuffer
) where

--import Data.List (maximum)
import Data.List.Split

import Types

makeCharBuffer :: Char -> Buffer
makeCharBuffer c = Buffer { getBufferContents = getCharBufferContents c }

getCharBufferContents :: Char -> String
getCharBufferContents c = concat $ take 4 (repeat (take 10 (repeat c) ++ "\n"))

renderBuffer :: Buffer -> (Int, Int) -> [String]
renderBuffer buf (w, h) = hPaddedLines
  where contents = getBufferContents buf
        lines = splitOn "\n" contents
        pad line = line ++ take (w - (length line)) (repeat ' ')
        paddedLines = map pad lines
        hPaddedLines = paddedLines ++ (take (h - length paddedLines) $ repeat blankLine)
        blankLine = take w (repeat ' ')
