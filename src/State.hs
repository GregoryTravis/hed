module State
( EditorState(..)
, ESAction
, io
) where

import Control.Monad.State

import Thing

data EditorState = EditorState { thing :: Thing }
  deriving (Eq, Show)

type ESAction a = StateT EditorState IO a

io :: IO a -> StateT t IO a
io = liftIO
