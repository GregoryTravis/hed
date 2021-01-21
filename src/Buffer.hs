module Buffer
( Buffer(..)
, Rendering
, gridToRendering ) where

import Data.Text hiding (map)
import qualified Data.Vector as V

import Event
import Util

class Buffer a where
  render :: a -> (Int, Int) -> Rendering
  update :: a -> Event -> a

data Rendering = Rendering [Text]

-- gridToRendering :: V.Vector (V.Vector a) -> (a -> Char) -> Rendering
-- -- TODO slow -- not sure how to speed this up
-- gridToRendering xses convert = Rendering texts
--   where texts = map toText $ V.toList xses
--         toText xs = pack (map convert $ V.toList xs)
