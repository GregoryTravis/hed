module EditorState
( initEditorState
, newWindow
) where

import qualified Data.Map as M

import Buffer
import Layout
import Types

initEditorState = EditorState
  { buffers = M.fromList []
  , currentBuffer = "scratch"
  , screenDim = Nothing
  , layout = EmptyLayout
  }

-- This should take a Window later
newWindow :: EditorState -> String -> Buffer -> EditorState
newWindow es name buf =
  es { buffers = M.insert name buf (buffers es),
       layout = addBufferToLayout (layout es) name }
