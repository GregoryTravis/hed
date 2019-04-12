module RectSampler
( sampleToString
) where

import Linear

type RectSampler = V2 Int -> V2 Int -> Char

sampleToString :: RectSampler -> V2 Int -> [Char]
sampleToString rs wh@(V2 width height) =
  concat $ map row [0..height-1]
  where row y = map (\x -> sample x y) [0..width-1]
        sample x y = rs wh (V2 x y)
