module State
( EditorState(..)
, ESAction
, io
) where

import Control.Monad.State

data EditorState = EditorState { char :: Maybe Char, count :: Int }
  deriving (Eq, Show)

type ESAction a = StateT EditorState IO a

io :: IO a -> StateT t IO a
io = liftIO
