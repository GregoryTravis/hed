module Types
( Layout(..)
, ESAction
, EditorState(..)
, Buffer(..)
, Window(..)
, WindowPlacement(..)
, KeyBinding(..)
) where

import Control.Monad.State
import qualified Data.Map as M

data Layout = Win Window | HStack Layout Layout | VStack Layout Layout | EmptyLayout
  deriving (Eq, Show)

type ESAction a = StateT EditorState IO a

data EditorState = EditorState
  { buffers :: M.Map String Buffer
  , screenDim :: Maybe (Int, Int)
  , layout :: Layout
  , currentWindowId :: Int
  , nextWindowId :: Int
  , debugStr :: String
  }
  deriving (Eq, Show)

-- id, name, cursorPos, origin
data Window = Window Int String Int (Int, Int)
  deriving (Eq, Show)

data Buffer = Buffer { bufferContents :: String }
  deriving (Eq)

instance Show Buffer where
  show (Buffer { bufferContents = bc }) = "Buffer \"" ++ (take 10 bc) ++ "\""

data WindowPlacement = WindowPlacement Window (Int, Int) (Int, Int)
  deriving (Eq, Show)

data KeyBinding = TransformBinding (EditorState -> EditorState) | ActionBinding (ESAction ())
