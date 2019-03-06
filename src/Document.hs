module Document
( Document(..)
, clipCursorToDocument
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
import Data.Word (Word8)
import qualified Data.Vector as V
import qualified Debug.Trace as TR

import FrameBuffer hiding (debug)
import Util

--debug = False
debug = True

data Document = Document (V.Vector ByteString) deriving (Eq, Show)

numLines (Document lines) = V.length lines
-- Does not do bounds-checking
lineLength (Document lines) lineNo = BS.length (lines ! lineNo)

type DocChar = Word8

charAt :: Document -> Int -> Int -> DocChar
--charAt _ x y | TR.trace (show ("hey", x, y)) False = undefined
charAt (Document lines) x y
  | x < 0 = conv ' '
  | y < 0 = conv ' '
  | y >= (V.length lines) = conv ' '
  | otherwise = ch x (lines ! y)
  where ch x line | x >= (BS.length line) = conv ' '
                  -- | x == (BS.length line) = conv ' '
                  | otherwise = BS.index line x
        conv c = BS.index (C8.pack [c]) 0

slowRenderDocument :: FrameBuffer -> Document -> ViewPos -> Builder
--slowRenderDocument fb doc vp | TR.trace (show (fb, doc, vp)) False = undefined
slowRenderDocument (FrameBuffer (w, h)) doc (ViewPos vx vy) =
  toBuilder $ [charAt doc (x + vx) (y + vy) | y <- [0..h-1], x <- [0..w-1]]
  where toBuilder :: [DocChar] -> Builder
        toBuilder ws = byteString $ BS.pack ws

-- Move cursor to be within the boundaries of the document.
-- Vertically, this means between the first and last lines (incl.)
-- Horizontally, it means between the first and last chars (incl.) or after the last
clipCursorToDocument :: Document -> (Int, Int) -> (Int, Int)
clipCursorToDocument doc (x, y)
  | y < 0 = clipCursorToDocument doc (x, 0)
  | y >= numLines doc = clipCursorToDocument doc (x, numLines doc - 1)
  | x < 0 = clipCursorToDocument doc (0, y)
  | x > ll = clipCursorToDocument doc (x, ll)
  | otherwise = (x, y)
  where ll = lineLength doc y

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

fastRenderDocument :: FrameBuffer -> Document -> ViewPos -> Builder
fastRenderDocument fb doc vp = renderDocumentAsBS fb doc vp lotsOfSpaces
  where lotsOfSpaces = blankLine fb

blankLine :: FrameBuffer -> ByteString
blankLine (FrameBuffer (w, h)) = C8.pack (replicate w ' ')

bothRenderDocument :: FrameBuffer -> Document -> ViewPos -> Builder
bothRenderDocument fb doc vp =
  let fast = fastRenderDocument fb doc vp
      slow = slowRenderDocument fb doc vp
      fastBs = (BSL.toStrict (B.toLazyByteString fast))
      slowBs = (BSL.toStrict (B.toLazyByteString slow))
   in assert (fastBs == slowBs) $
        byteString fastBs

renderDocument = if debug then bothRenderDocument else fastRenderDocument
