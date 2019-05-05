module Layout
( Layout(..)
, renderLayout
, addBufferToLayout
, getWindows
, getWindowIds
, getWindow
, hasWindow
, getWindowPlacement
, replaceWindow
, textCursorPosToXY
, textXYToCursorPos
, getWindowUL
, getAbsoluteCusorPos
) where

import Data.List (find)
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe

import Buffer
import Types
import Util

getWindowArrangement :: Layout -> (Int, Int) -> (Int, Int) -> [WindowPlacement]
getWindowArrangement (Win win) pos dim = [WindowPlacement win pos dim]
getWindowArrangement (VStack top bottom) (x, y) (w, h) = topA ++ bottomA
  where topA = getWindowArrangement top (x, y) (w, topH)
        bottomA = getWindowArrangement bottom (x, y + topH + 1) (w, bottomH)
        topH = (h-1) `div` 2
        bottomH = h - topH - 1
getWindowArrangement (HStack left right) (x, y) (w, h) = leftA ++ rightA
  where leftA = getWindowArrangement left (x, y) (leftW, h)
        rightA = getWindowArrangement right (x + leftW + 1, y) (rightW, h)
        leftW = (w-1) `div` 2
        rightW = w - leftW - 1
getWindowArrangement EmptyLayout pos dim = []

getWindowPlacement :: EditorState -> Int -> WindowPlacement
getWindowPlacement (EditorState { screenDim = dim, layout = layout }) windowId = fromJust $ find (withId windowId) (getWindowArrangement layout (0, 0) (fromJust dim))
  where withId anId (WindowPlacement (Window id _ _ _) _ _) = anId == id

--renderLayout :: EditorState -> Layout -> (Int, Int) -> [String]
renderLayout es (Win (Window _ bufferName _ origin)) dim = renderBuffer (buffers es M.! bufferName) origin dim
renderLayout es (VStack top bottom) (w, h) = vConcat w topR bottomR
  where topR = renderLayout es top (w, topH)
        bottomR = renderLayout es bottom (w, bottomH)
        topH = (h-1) `div` 2
        bottomH = h - topH - 1
renderLayout es (HStack left right) (w, h) = hConcat leftR rightR
  where leftR = renderLayout es left (leftW, h)
        rightR = renderLayout es right (rightW, h)
        leftW = (w-1) `div` 2
        rightW = w - leftW - 1
renderLayout es EmptyLayout (w, h) = take h (repeat (take w (repeat '.')))

getWindows :: Layout -> [Window]
getWindows (Win w) = [w]
getWindows (VStack top bottom) = (getWindows top) ++ (getWindows bottom)
getWindows (HStack left right) = (getWindows left) ++ (getWindows right)
getWindows EmptyLayout = []

getWindowIds layout = map getId (getWindows layout)
  where getId (Window id _ _ _) = id

getWindow layout windowId = fromJust $ find (\w -> getId w == windowId) (getWindows layout)
  where getId (Window id _ _ _) = id

getWindowBuffer es windowId =
  case getWindow (layout es) windowId of Window _ name _ _ -> buffers es M.! name

hasWindow layout windowId = elem windowId (getWindowIds layout)

replaceWindow :: Layout -> Window -> Layout
replaceWindow layout w@(Window id _ _ _) =
  let ok = hasWindow layout id
      layout' = replaceWindow' layout w
   in assertM "" ok layout'
  where replaceWindow' ww@(Win (Window id' _ _ _)) w@(Window id _ _ _) | id == id' = (Win w)
                                                                       | otherwise = ww
        replaceWindow' (VStack t b) w = VStack (replaceWindow' t w) (replaceWindow' b w)
        replaceWindow' (HStack l r) w = HStack (replaceWindow' l w) (replaceWindow' r w)
        replaceWindow' EmptyLayout _ = EmptyLayout

vConcat width top bottom = top <> [(take width (repeat '-'))] <> bottom
hConcat lefts rights = map pc $ zip lefts rights
  where pc (l, r) = l ++ "|" ++ r

addBufferToLayout :: Layout -> Int -> String -> Layout
addBufferToLayout EmptyLayout id name = Win (Window id name 0 (0, 0))
addBufferToLayout layout id name = VStack layout (Win (Window id name 66 (0, 0)))

-- Turn 2d position of a 1d cursor position
textCursorPosToXY :: EditorState -> Int -> Int -> (Int, Int)
textCursorPosToXY es windowId cursorPos =
  let buffer = getWindowBuffer es windowId
      lines = splitOn "\n" $ take cursorPos $ bufferContents buffer
      y = length lines - 1
      x = length (lines !! y)
   in (x, y)

-- Turn a 2d position to a 1d cursor position.  If the 2d position falls in
-- empty space then move it back to the end of its line; if it is beyond the
-- end of the buffer then put it at the end
textXYToCursorPos :: EditorState -> Int -> (Int, Int) -> Int
textXYToCursorPos es windowId (x, y) =
  let buffer = getWindowBuffer es windowId
      lines :: [String]
      lines = splitOn "\n" $ bufferContents buffer
      voo :: [String]
      voo = take 2 lines
      yeah :: Int -> Int -> Int
      aboveLen = length (concat (take y lines)) + y
      yeah x y | x < 0 || y < 0 = 0
               | y >= (length lines) = bufferLen - 1
               | x > lineLen = aboveLen + lineLen
               | otherwise = aboveLen + x
      lineLen = length $ (lines !! y)
      bufferLen = length $ bufferContents buffer
   in yeah x y

getWindowUL :: EditorState -> Int -> (Int, Int)
getWindowUL es windowId =
  case getWindowPlacement es windowId of (WindowPlacement win pos dim) -> pos

getAbsoluteCusorPos es = do
  let Window _ name cursorPos (ox, oy) = getWindow (layout es) (currentWindowId es)
      (x, y) = getWindowUL es (currentWindowId es)
      (dx, dy) = textCursorPosToXY es (currentWindowId es) cursorPos
   in (x + dx - ox, y + dy - oy)
