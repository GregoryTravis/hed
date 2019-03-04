{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (finally, catch, IOException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Builder as B
import Data.Char (chr, ord)
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Control.Monad.State
import System.Console.ANSI
import System.IO
import System.Posix.IO (fdRead, stdInput)
import System.Posix.Terminal

import Util

box w h c = replicate h (pack (replicate w c))

drawBox (startX, startY) box = do
  mapM_ drawRow (zip [0..] box)
  where drawRow (y, row) = do setCursorPosition (startY + y) startX
                              putStr (unpack row)

-- Taken from https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input/36297897#36297897
withRawInput :: Int -> Int -> IO a -> IO a
withRawInput vmin vtime application = do

  {- retrieve current settings -}
  oldTermSettings <- getTerminalAttributes stdInput

  {- modify settings -}
  let newTermSettings = 
        flip withoutMode  EnableEcho   . -- don't echo keystrokes
        flip withoutMode  ProcessInput . -- turn on non-canonical mode
        flip withTime     vtime        . -- wait at most vtime decisecs per read
        flip withMinInput vmin         $ -- wait for >= vmin bytes per read
        oldTermSettings

  {- install new settings -}
  setTerminalAttributes stdInput newTermSettings Immediately

  {- restore old settings no matter what; this prevents the terminal
   - from becoming borked if the application halts with an exception
   -}
  application 
    `finally` do setTerminalAttributes stdInput oldTermSettings Immediately
                 msp "ASDFASDF"
                 return ()

data Document = Document (V.Vector ByteString) deriving (Eq, Show)

readFileAsDoc :: String -> IO Document
readFileAsDoc filename = do
  entireFile <- readFile filename
  return $ Document $ V.fromList $ BS.split (head (BS.unpack (C8.pack "\n"))) (C8.pack entireFile)

-- Position of the cursor relative to the screen origin
data CursorPos = CursorPos Int Int deriving (Eq, Show)
-- Position of the text character at the screen origin
data ViewPos = ViewPos Int Int deriving (Eq, Show)

data ViewState = ViewState ViewPos CursorPos deriving (Eq, Show)

type Generation = Int

-- Int: generation
data EditorState = EditorState Generation ViewState Document deriving (Eq, Show)

generationOf (EditorState generation _ _) = generation

readFileInitState filename = do
  doc <- readFileAsDoc filename
  return $ EditorState 0 (ViewState (ViewPos 0 0) (CursorPos 0 0)) doc

readKeystrokes :: IO [Char]
readKeystrokes = do
  ready <- hReady stdin
  --() <- if ready then msp ("ready", ready) else return ()
  if ready then do c <- hGetChar stdin
                   --msp ("lip", c)
                   rest <- readKeystrokes
                   return (c:rest)
          else return []

debug fb@(FrameBuffer (w, h)) s = do
  setCursorPosition (h-1) 0
  putStr s

-- Hstrip text (start, lenth) (x, y)
data Hstrip = Hstrip Text (Int, Int) (Int, Int)

-- FrameBuffer (w, h)
data FrameBuffer = FrameBuffer (Int, Int)

getFrameBuffer = do
  Just (h, w) <- getTerminalSize
  return $ FrameBuffer (w, h)

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
   in mconcat $ map (renderDocumentLineAsBS fb vx lotsOfSpaces) linesOnScreen

renderDocument :: FrameBuffer -> ViewPos -> Document -> IO ()
renderDocument fb vp doc = do
  setCursorPosition 0 0
  B.hPutBuilder stdout $ renderDocumentAsBS fb doc vp lotsOfSpaces
  hFlush stdout
  where lotsOfSpaces = blankLine fb

blankLine :: FrameBuffer -> ByteString
blankLine (FrameBuffer (w, h)) = C8.pack (replicate w ' ')

render fb (EditorState _ (ViewState vp cp) doc) = do
  --clearScreen
  renderDocument fb vp doc
  hFlush stdout

data Command = Dir Int Int | Huh String deriving (Eq, Show)

keystrokeToCommand :: Char -> Command
keystrokeToCommand 'h' = Dir (-1) 0
keystrokeToCommand 'l' = Dir 1 0
keystrokeToCommand 'j' = Dir 0 1
keystrokeToCommand 'k' = Dir 0 (-1)
keystrokeToCommand c = Huh [c]

getEditorState :: State EditorState EditorState
getEditorState = state $ \es -> (es, es)
setEditorState :: EditorState -> State EditorState ()
setEditorState es' = state $ \es -> ((), es')

processCommand :: FrameBuffer -> Command -> State EditorState ()
processCommand fb (Dir dx dy) = do
  es@(EditorState generation (ViewState (ViewPos x y) cp) doc) <- getEditorState
  let newViewPos = clipToFB fb $ ViewPos (x + dx) (y + dy)
  setEditorState $ EditorState (generation + 1) (ViewState newViewPos cp) doc
processCommand fb (Huh _) = return ()

clipToFB (FrameBuffer (w, h)) (ViewPos x y) = ViewPos (clip x 0 w) (clip y 0 h)
clip x lo hi
  | x < lo = lo
  | x >= hi-1 = hi-1
  | otherwise = x

processKeys :: FrameBuffer -> [Char] -> State EditorState ()
processKeys fb keys = mapM_ (\key -> processCommand fb (keystrokeToCommand key)) keys

processKeysReturnState es fb keys = case (runState (processKeys fb keys) es) of ((), es') -> es'

editorLoop es fb generation = do
  --msp "loop"
  keystrokes <- readKeystrokes
  --let keystrokes = [] :: [Char]
  () <- if ((length keystrokes) == 0) then (return ()) else (debug fb keystrokes)
  let es' = processKeysReturnState es fb keystrokes
      needsRedraw = case generation of Just oldGeneration -> oldGeneration /= (generationOf es')
                                       Nothing -> True
  --msp needsRedraw
  () <- if needsRedraw then render fb es' else return ()
  editorLoop es' fb (Just $ generationOf es')

editorLoopStart es = do
  fb <- getFrameBuffer
  editorLoop es fb Nothing

main = do
  hSetBuffering stdin NoBuffering
  --hSetBuffering stdout NoBuffering
  setSGR [Reset]
  es <- readFileInitState "sample.txt"
  withRawInput 0 1 $ editorLoopStart es
  putStrLn "done2"
  msp "done"

fake :: Int -> String
fake o = concat (map (\i -> (replicate 158 (chr (97 + (mod (o+i) 26))))) [0..42-1])
fakes :: Vector String
fakes = V.fromList $ map fake [0..25]
fakesBs :: Vector ByteString
fakesBs = V.fromList $ map C8.pack $ map fake [0..25]
fakeLine i o = replicate 158 (chr (97 + (mod (o+i) 26)))
fakeBsSep o = mconcat (map byteString (map C8.pack (map (\i -> (replicate 158 (chr (97 + (mod (o+i) 26))))) [0..42-1])))
fakesBsSep :: Vector B.Builder
fakesBsSep = V.fromList $ map fakeBsSep [0..25]

speedTest label anim = do
  clearScreen
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout (BlockBuffering Nothing)
  time ("\n" ++ label ++ "\n")$ mapM_ foo [0..9999]
  --threadDelay $ 5 * 1000000
  where foo i = do setCursorPosition 0 0
                   anim i
                   hFlush stdout

speedTests = do
  speedTest "hPutBuilder BS" $ \i -> B.hPutBuilder stdout $ fakesBsSep ! (mod i 26)
  speedTest "hPutBuilder BSs" $ \i -> B.hPutBuilder stdout $ byteString (fakesBs ! (mod i 26))
  speedTest "putStr Text" $ \i -> putStr (fakes ! (mod i 26))

--main = speedTests

__main = do
  clearScreen
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout (BlockBuffering Nothing)
  time "putStr Text" $ mapM_ foo [0..9999]
  --threadDelay $ 5 * 1000000
  where foo i = do --clearScreen
                   setCursorPosition 0 0
                   putStr (fakes ! (mod i 26))
                   hFlush stdout
