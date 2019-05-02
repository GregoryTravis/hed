module EditorState
( initEditorState
--, newFileBuffer
, openFile
, newWindow
) where

import Control.Monad.State
import qualified Data.Map as M

import Buffer
import Control
import Layout
import Types

initEditorState = EditorState
  { buffers = M.fromList [("scratch", Buffer "hi")]
  , screenDim = Nothing
  , layout = Win (Window "scratch" (0, 0))
  }

transformEditorState f = do
  es <- get
  put (f es)

addBuffer es name buf = es { buffers = M.insert name buf (buffers es) }

-- This should take a Window later
newWindow :: String -> ESAction ()
newWindow name = transformEditorState $ \es -> es { layout = addBufferToLayout (layout es) name }

newFileBuffer :: String -> ESAction ()
newFileBuffer filename = do
  es <- get
  s <- io $ readFile filename
  let buf = Buffer { bufferContents = s }
  put $ addBuffer es filename buf

openFile filename = do
  newFileBuffer filename
  newWindow filename
