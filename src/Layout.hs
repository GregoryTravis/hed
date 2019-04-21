module Layout
( Layout(..)
, renderLayout
) where

import qualified Data.Map as M

import Buffer
import State

data Layout = Buf String | HStack Layout Layout | VStack Layout Layout

renderLayout :: EditorState -> Layout -> (Int, Int) -> [String]
renderLayout es (Buf bufferName) dim = renderBuffer (buffers es M.! bufferName) dim
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

vConcat width top bottom = top <> [(take width (repeat '-'))] <> bottom
hConcat lefts rights = map pc $ zip lefts rights
  where pc (l, r) = l ++ "|" ++ r
