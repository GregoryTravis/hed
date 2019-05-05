module Event (Event(..)) where

data Event =
  KeyEvent Char |
  ResizeEvent |
  GotWindowSizeEvent (Int, Int) |
  RedisplayEvent Char |
  QuitEvent |
  StateChangedEvent |
  RequestTransform String (String -> String)

instance Show Event where
  show (RequestTransform s f) = "(RequestTransform " ++ s ++ ")"
  --show 
