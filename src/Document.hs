module Document
( Document(..)
, clipCursorToDocument
, docToListAt
, insertCharInDoc
, readFileAsDoc
, renderDocument
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Builder as B
import Data.Char (chr)
import Data.Text (Text, pack, unpack)
import Data.Vector (Vector, (!), (//))
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

charToWord8 c = BS.index (C8.pack [c]) 0
word8ToChar = chr . fromEnum

docCharAt :: Document -> Int -> Int -> DocChar
--docCharAt _ x y | TR.trace (show ("hey", x, y)) False = undefined
docCharAt (Document lines) x y
  | x < 0 = conv ' '
  | y < 0 = conv ' '
  | y >= (V.length lines) = conv ' '
  | otherwise = ch x (lines ! y)
  where ch x line | x > (BS.length line) = conv ' '
                  | x == (BS.length line) = conv '\n'
                  | otherwise = BS.index line x
        conv = charToWord8

charAt doc x y = word8ToChar $ docCharAt doc x y

-- Find the first location that satisfies the predicate
--lookFor :: Document -> (Int, Int) -> ([Char] -> Bool) -> (Int, Int)

docToListAt :: Document -> (Int, Int) -> [Char]
docToListAt doc (x, y)
  | atDocEnd doc (x, y) = []
  -- | otherwise = (eesp (show ("um", x, y, (charAt doc x y))) (charAt doc x y)) : (eesp (show ("tR", theRest)) theRest)
  | otherwise = (charAt doc x y) : theRest
  where theRest = docToListAt doc (moveCursor doc (x, y) 1)

--atDocEnd (Document lines) (x, y) | TR.trace (show ("heyy", x, y, V.length lines)) False = undefined
atDocEnd (Document lines) (x, y)
  | x == 0 && y == (V.length lines) = True
  | y < (V.length lines) = False

moveCursor :: Document -> (Int, Int) -> Int -> (Int, Int)
--clipCursorToDocument _ (x, y) | TR.trace (lesp "higgs" (show ("ah", x, y))) False = undefined
moveCursor doc (x, y) dx
  | dx == 0 = (x, y)
  | dx > 0 = let len = lineLength doc y
              in if x > len -- not >=, because of the newline
                   then moveCursor doc (0, y+1) (dx-1)
                   else moveCursor doc (x + 1, y) (dx-1)

slowRenderDocument :: FrameBuffer -> Document -> ViewPos -> Builder
--slowRenderDocument fb doc vp | TR.trace (show (fb, doc, vp)) False = undefined
slowRenderDocument (FrameBuffer (w, h)) doc (ViewPos vx vy) =
  toBuilder $ [charToWord8 $ vis $ word8ToChar $ docCharAt doc (x + vx) (y + vy) | y <- [0..h-1], x <- [0..w-1]]
  where toBuilder :: [DocChar] -> Builder
        toBuilder ws = byteString $ BS.pack ws
        -- Show newline as space
        vis '\n' = ' '
        vis c = c

-- Move cursor to be within the boundaries of the document.
-- Vertically, this means between the first and last lines (incl.)
-- Horizontally, it means between the first and last chars (incl.) or after the last
clipCursorToDocument :: Document -> (Int, Int) -> (Int, Int)
--clipCursorToDocument _ (x, y) | TR.trace (lesp "higgs" (show ("ah", x, y))) False = undefined
clipCursorToDocument doc (x, y)
  | y < 0 = clipCursorToDocument doc (x, 0)
  | y >= numLines doc = clipCursorToDocument doc (x, numLines doc - 1)
  | x < 0 = clipCursorToDocument doc (0, y)
  | x > ll = clipCursorToDocument doc (ll, y)
  | otherwise = (x, y)
  where ll = lineLength doc y

insertIntoLine :: ByteString -> Int -> Char -> ByteString
insertIntoLine bs pos c = newBs
  where lineList = BS.unpack bs
        newLineList = (take pos lineList) ++ [(charToWord8 c)] ++ (drop pos lineList)
        newBs = BS.pack newLineList

insertCharInDoc (Document lines) x y c = Document newLines
  where newLines = lines // [(y, newLine)]
        newLine = insertIntoLine (lines ! y) x c

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
