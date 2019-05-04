module Buffer
( Buffer(..)
, renderBuffer
, appendToBuffer
, getCursorRelativeOffset
, textCursorPosToXY
, textXYToCursorPos
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

-- Turn 2d position of a 1d cursor position
textCursorPosToXY :: Buffer -> Int -> (Int, Int)
textCursorPosToXY buffer cursorPos =
  let lines = splitOn "\n" $ take cursorPos $ bufferContents buffer
      y = length lines - 1
      x = length (lines !! y)
   in (x, y)

-- Turn a 2d position to a 1d cursor position.  If the 2d position falls in
-- empty space then move it back to the end of its line; if it is beyond the
-- end of the buffer then put it at the end
textXYToCursorPos :: Buffer -> (Int, Int) -> Int
textXYToCursorPos buffer (x, y) =
  let lines :: [String]
      lines = splitOn "\n" $ bufferContents buffer
      voo :: [String]
      voo = take 2 lines
      yeah :: Int -> Int -> Int
      yeah x y | x < 0 || y < 0 = 0
               | y >= (length lines) = bufferLen - 1
               | x > lineLen = length (concat (take y lines)) + lineLen
               | otherwise = length (concat (take y lines)) + x
      lineLen = length $ (lines !! y)
      bufferLen = length $ bufferContents buffer
   in yeah x y
