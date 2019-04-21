module State
( EditorState(..)
, ESAction
, initEditorState
, io
) where

import Control.Monad.State
import qualified Data.Map as M

import Buffer

data EditorState = EditorState
  { buffers :: M.Map String Buffer
  , currentBuffer :: String
  , screenDim :: Maybe (Int, Int)
  }
  deriving (Eq, Show)

initEditorState = EditorState
  { buffers = M.fromList [("scratch", Buffer 'a'), ("hey", Buffer 'q')]
  , currentBuffer = "scratch"
  , screenDim = Nothing
  }

type ESAction a = StateT EditorState IO a

io :: IO a -> StateT t IO a
io = liftIO
