module Buffer
( Buffer(..)
, renderBuffer
) where

data Buffer = Buffer Char
  deriving (Eq, Show)

renderBuffer :: Buffer -> (Int, Int) -> String
renderBuffer (Buffer c) (w, h) = blank <> mconcat lines <> blank
  where blank = take w (repeat ' ')
        lines = take (h-2) (repeat line)
        line = " " <> (take (w-2) (repeat c)) <> " "
