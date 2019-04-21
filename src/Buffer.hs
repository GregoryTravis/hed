module Buffer
( Thing(..)
, renderThing
) where

data Thing = Thing Char
  deriving (Eq, Show)

renderThing :: Thing -> (Int, Int) -> String
renderThing (Thing c) (w, h) = blank <> mconcat lines <> blank
  where blank = take w (repeat ' ')
        lines = take (h-2) (repeat line)
        line = " " <> (take (w-2) (repeat c)) <> " "
