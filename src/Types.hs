module Types
( Layout(..)
, EditorState(..)
, Buffer(..)
, Window(..)
, WindowPlacement(..)
) where

import qualified Data.Map as M

data Layout = Win Window | HStack Layout Layout | VStack Layout Layout | EmptyLayout
  deriving (Eq, Show)

data EditorState = EditorState
  { buffers :: M.Map String Buffer
  , screenDim :: Maybe (Int, Int)
  , layout :: Layout
  , currentWindowId :: Int
  , nextWindowId :: Int
  , debugStr :: String
  }
  deriving (Eq, Show)

data Window = Window Int String (Int, Int)
  deriving (Eq, Show)

data Buffer = Buffer { bufferContents :: String }
  deriving (Eq)

instance Show Buffer where
  show (Buffer { bufferContents = bc }) = "Buffer \"" ++ (take 10 bc) ++ "\""

data WindowPlacement = WindowPlacement Window (Int, Int) (Int, Int)
  deriving (Eq, Show)
