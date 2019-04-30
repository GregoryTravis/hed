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
  { buffers = M.fromList [("scratch", makeCharBuffer 'a'), ("hey", makeStringBuffer "zxcv")]
  , currentBuffer = "scratch"
  , screenDim = Nothing
  , layout = (VStack (Buf "hey") (Buf "scratch"))
  }

type ESAction a = StateT EditorState IO a

io :: IO a -> StateT t IO a
io = liftIO
