module State
( EditorState(..)
, io
, asAction
) where

import Control.Monad.State

import Types

io :: IO a -> StateT t IO a
io = liftIO

asAction :: (EditorState -> EditorState) -> ESAction ()
asAction f = do
  es <- get
  put $ f es
