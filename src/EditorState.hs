module EditorState
( initEditorState
--, newFileBuffer
, esaction
, openFile
, saveFile
, newWindow
, switchToWindow
, nextWindow
, moveCursor
) where

import Control.Monad.State
import qualified Data.Map as M
import Data.List.Split
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

saveFile filename contents = writeFile filename contents

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

-- Scroll window so that cursor is inside the visible area
moveOriginToShowCursor es newCursorPos x y =
  let WindowPlacement win pos (w, h) = getWindowPlacement es (currentWindowId es)
      Window id name cursorPos (ox, oy) = getWindow (layout es) (currentWindowId es)
      (x, y) = textCursorPosToXY es (currentWindowId es) newCursorPos
      nox | x < ox = x
          | x-ox >= w = ox - ((x-ox)-w-1)
          | otherwise = ox
      noy | y < oy = y
          | y-oy >= h = oy - ((y-oy)-h-1)
          | otherwise = oy
   in (nox, noy)

moveCursor :: EditorState -> Int -> Int -> EditorState
moveCursor es dx dy =
  let lines = splitOn "\n" $ bufferContents currentBuffer
      currentBuffer = (buffers es) M.! name
      (Window id name cursorPos origin) = getWindow (layout es) (currentWindowId es)
      (x, y) = textCursorPosToXY es (currentWindowId es) cursorPos
      cursorPos' = textXYToCursorPos es (currentWindowId es) (x + dx, y + dy)
      (nx, ny) = textCursorPosToXY es (currentWindowId es) cursorPos'
      window' = Window id name cursorPos' origin'
      origin' = moveOriginToShowCursor es cursorPos' nx ny
   in es { layout = replaceWindow (layout es) window'
         , debugStr = show ((x, y), (x+dx, y+dy), (nx, ny), origin, origin', cursorPos, cursorPos') }
