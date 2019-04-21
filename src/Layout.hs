module Layout
( Layout(..)
, renderLayout
) where

import Buffer

data Layout = Buf Buffer | HStack Layout Layout | VStack Layout Layout

renderLayout :: Layout -> (Int, Int) -> [String]
renderLayout (Buf buffer) dim = renderBuffer buffer dim
renderLayout (VStack top bottom) (w, h) = vConcat w topR bottomR
  where topR = renderLayout top (w, topH)
        bottomR = renderLayout bottom (w, bottomH)
        topH = (h-1) `div` 2
        bottomH = h - topH - 1
renderLayout (HStack left right) (w, h) = hConcat leftR rightR
  where leftR = renderLayout left (leftW, h)
        rightR = renderLayout right (rightW, h)
        leftW = (w-1) `div` 2
        rightW = w - leftW - 1

vConcat width top bottom = top <> [(take width (repeat '-'))] <> bottom
hConcat lefts rights = map pc $ zip lefts rights
  where pc (l, r) = l ++ "|" ++ r
