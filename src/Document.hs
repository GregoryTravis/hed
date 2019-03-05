module Document
( Document(..)
, readFileAsDoc
, renderDocument
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Builder as B
import Data.Text (Text, pack, unpack)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import FrameBuffer
import Util

data Document = Document (V.Vector ByteString) deriving (Eq, Show)

readFileAsDoc :: String -> IO Document
readFileAsDoc filename = do
  entireFile <- readFile filename
  return $ Document $ V.fromList $ BS.split (head (BS.unpack (C8.pack "\n"))) (C8.pack entireFile)

-- Trim a line at an x offset to the screen width, and add enough spaces to equal the screen width
renderDocumentLineAsBS :: FrameBuffer -> Int -> ByteString -> ByteString -> Builder
renderDocumentLineAsBS (FrameBuffer (w, h)) vx lotsOfSpaces line =
  let clippedLine = BS.take w $ BS.drop (fromIntegral vx) line
      spacesNeeded = max 0 (w - (fromIntegral $ BS.length clippedLine))
      spaces = if spacesNeeded == 0 then mempty else BS.take (fromIntegral spacesNeeded) lotsOfSpaces
      combined = byteString clippedLine <> byteString spaces
      ok = (BS.length clippedLine + BS.length spaces) == fromIntegral w
      ok2 = (BS.length (BSL.toStrict (B.toLazyByteString combined))) == fromIntegral w
   in assert (ok && ok2) combined

renderDocumentAsBS :: FrameBuffer -> Document -> ViewPos -> ByteString -> Builder
renderDocumentAsBS fb@(FrameBuffer (w, h)) (Document lines) (ViewPos vx vy) lotsOfSpaces =
  let linesOnScreen = take h (drop vy (V.toList lines))
      additionalBlankLines = h - (length linesOnScreen)
   in (mconcat $ map (renderDocumentLineAsBS fb vx lotsOfSpaces) linesOnScreen) <>
     mconcat (map byteString (replicate additionalBlankLines lotsOfSpaces))

renderDocument :: FrameBuffer -> ViewPos -> Document -> Builder
renderDocument fb vp doc = renderDocumentAsBS fb doc vp lotsOfSpaces
  where lotsOfSpaces = blankLine fb

blankLine :: FrameBuffer -> ByteString
blankLine (FrameBuffer (w, h)) = C8.pack (replicate w ' ')

