module Layout
( Layout(..)
, renderLayout
, addBufferToLayout
, getWindows
, getWindowIds
, hasWindow
, getWindowPlacement
) where

import Data.List (find)
import Data.Maybe
import qualified Data.Map as M

import Buffer
import Types

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

getWindowPlacement :: EditorState -> Int-> WindowPlacement
getWindowPlacement (EditorState { screenDim = dim, layout = layout }) windowId = fromJust $ find (withId windowId) (getWindowArrangement layout (0, 0) (fromJust dim))
  where withId anId (WindowPlacement (Window id _ _) _ _) = anId == id

--renderLayout :: EditorState -> Layout -> (Int, Int) -> [String]
renderLayout es (Win (Window _ bufferName origin)) dim = renderBuffer (buffers es M.! bufferName) origin dim
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
  where getId (Window id _ _) = id

hasWindow layout windowId = elem windowId (getWindowIds layout)

vConcat width top bottom = top <> [(take width (repeat '-'))] <> bottom
hConcat lefts rights = map pc $ zip lefts rights
  where pc (l, r) = l ++ "|" ++ r

addBufferToLayout :: Layout -> Int -> String -> Layout
addBufferToLayout EmptyLayout id name = Win (Window id name (0, 0))
addBufferToLayout layout id name = VStack layout (Win (Window id name (0, 0)))
