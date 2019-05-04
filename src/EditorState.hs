module EditorState
( initEditorState
--, newFileBuffer
, esaction
, openFile
, newWindow
, switchToWindow
, nextWindow
, getCursorPos
, moveCursor
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
  , layout = Win (Window 0 "scratch" 0 (0, 0))
  , currentWindowId  = 0
  , nextWindowId = 1
  , debugStr = "debug"
  }

esaction f = do
  es <- get
  put (f es)

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

switchToWindow :: EditorState -> Int -> EditorState
switchToWindow es windowId =
  let es' = es { currentWindowId = windowId }
      ok = hasWindow (layout es) windowId
   in assertM "no such window" ok es'

nextWindow es =
  let wids = getWindowIds (layout es)
      current = currentWindowId es
      nextWindowId = fromJust $ valueAfterCyclic wids current
   in switchToWindow es nextWindowId

getCursorPos :: EditorState -> Int -> (Int, Int)
getCursorPos es windowId =
  case getWindowPlacement es windowId of (WindowPlacement win pos dim) -> pos

moveCursor :: EditorState -> Int -> EditorState
moveCursor es delta = es { layout = layout' }
  where layout' = replaceWindow (layout es) window'
        Window id name cursorPos origin = getWindow (layout es) (currentWindowId es)
        window' = Window id name (fix (cursorPos + delta)) origin
        buffer = (buffers es) M.! name
        bufferLen = length (bufferContents buffer)
        fix x | x < 0 = 0
              | x >= bufferLen - 1 = bufferLen - 1
              | otherwise = x
