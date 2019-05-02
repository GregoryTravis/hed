module EditorState
( initEditorState
--, newFileBuffer
, openFile
, newWindow
, switchToWindow
, nextWindow
) where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

import Buffer
import Control
import Layout
import Types
import Util

initEditorState = EditorState
  { buffers = M.fromList [("scratch", Buffer "hi\n")]
  , screenDim = Nothing
  , layout = Win (Window 0 "scratch" (0, 0))
  , currentWindowId  = 0
  , nextWindowId = 1
  , debugStr = "debug"
  }

--transformEditorState f = do
  --es <- get
  --put (f es)

addBuffer es name buf = es { buffers = M.insert name buf (buffers es) }

-- This should take a Window later
newWindow :: String -> ESAction ()
newWindow name = do
  es <- get
  let newWindowId = nextWindowId es
  put $ es { layout = addBufferToLayout (layout es) newWindowId name,
             nextWindowId = nextWindowId es + 1 }

newFileBuffer :: String -> ESAction ()
newFileBuffer filename = do
  es <- get
  s <- io $ readFile filename
  let buf = Buffer { bufferContents = s }
  put $ addBuffer es filename buf

openFile filename = do
  newFileBuffer filename
  newWindow filename

switchToWindow :: Int -> ESAction ()
switchToWindow windowId = do
  es <- get
  let es' = es { currentWindowId = windowId }
  let ok = hasWindow (layout es) windowId
  put $ assertM "no such window" ok es'

nextWindow :: ESAction ()
nextWindow = do
  es <- get
  let wids = getWindowIds (layout es)
      current = currentWindowId es
      nextWindowId = fromJust $ valueAfterCyclic wids current
  switchToWindow nextWindowId
