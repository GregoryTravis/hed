module State
( EditorState(..)
, ESAction
, initEditorState
, io
) where

import Control.Monad.State
import qualified Data.Map as M

import Buffer
import Types

initEditorState = EditorState
  { buffers = M.fromList [("scratch", Buffer 'a'), ("hey", Buffer 'q')]
  , currentBuffer = "scratch"
  , screenDim = Nothing
  , layout = (VStack (Buf "scratch") (Buf "hey"))
  }

type ESAction a = StateT EditorState IO a

io :: IO a -> StateT t IO a
io = liftIO
