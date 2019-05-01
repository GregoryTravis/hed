module State
( EditorState(..)
, ESAction
, io
) where

import Control.Monad.State

import Types

type ESAction a = StateT EditorState IO a

io :: IO a -> StateT t IO a
io = liftIO
