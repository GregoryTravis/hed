module Types
( Layout(..)
, EditorState(..)
, Buffer(..)
) where

import qualified Data.Map as M

data Layout = Buf String | HStack Layout Layout | VStack Layout Layout | EmptyLayout
  deriving (Eq, Show)

data EditorState = EditorState
  { buffers :: M.Map String Buffer
  , currentBuffer :: String
  , screenDim :: Maybe (Int, Int)
  , layout :: Layout
  }
  deriving (Eq, Show)

data Buffer = Buffer { bufferContents :: String }
  deriving (Eq, Show)
