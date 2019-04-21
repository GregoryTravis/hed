module Thing
( Thing(..)
, renderThing
) where

data Thing = Thing Char
  deriving (Eq, Show)

renderThing :: Thing -> (Int, Int) -> String
renderThing (Thing c) (w, h) = take (w * h) (repeat c)
