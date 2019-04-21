module State
( EditorState(..)
, ESAction
, initEditorState
, io
) where

import Control.Monad.State

import Thing

data EditorState = EditorState { thing :: Thing, screenDim :: Maybe (Int, Int) }
  deriving (Eq, Show)

initEditorState = EditorState { thing = Thing 'a', screenDim = Nothing }

type ESAction a = StateT EditorState IO a

io :: IO a -> StateT t IO a
io = liftIO
