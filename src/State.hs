module State
( EditorState(..)
, ESAction
, io
) where

import Control.Monad.State

newtype EditorState = EditorState { stack :: [Integer] }

type ESAction a = StateT EditorState IO a

io :: IO a -> StateT t IO a
io = liftIO
