module Event (Event(..)) where

data Event =
  KeyEvent Char |
  ResizeEvent |
  GotWindowSizeEvent (Int, Int) |
  RedisplayEvent Char |
  QuitEvent |
  StateChangedEvent
  deriving (Show)