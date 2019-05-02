module Layout
( Layout(..)
, renderLayout
, addBufferToLayout
) where

import qualified Data.Map as M

import Buffer
import Types

--renderLayout :: EditorState -> Layout -> (Int, Int) -> [String]
renderLayout es (Win (Window _ bufferName offset)) dim = renderBuffer (buffers es M.! bufferName) offset dim
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

vConcat width top bottom = top <> [(take width (repeat '-'))] <> bottom
hConcat lefts rights = map pc $ zip lefts rights
  where pc (l, r) = l ++ "|" ++ r

addBufferToLayout :: Layout -> Int -> String -> Layout
addBufferToLayout EmptyLayout id name = Win (Window id name (0, 0))
addBufferToLayout layout id name = VStack layout (Win (Window id name (0, 0)))
