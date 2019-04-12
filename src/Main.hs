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
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Vector (Vector, (!))
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Control.Monad.State
import System.Console.ANSI
import System.IO
import System.Posix.IO (fdRead, stdInput)
import System.Posix.Terminal

import Document
import FrameBuffer
import Util

busyLoop = False

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

-- Position of the cursor relative to the screen origin
data CursorPos = CursorPos Int Int deriving (Eq, Show)

data ViewState = ViewState ViewPos CursorPos deriving (Eq, Show)

data EditorState = EditorState {
  viewState :: ViewState,
  document :: Document } deriving (Eq, Show)

readFileInitState filename = do
  doc <- readFileAsDoc filename
  return $ EditorState { viewState = (ViewState (ViewPos 0 0) (CursorPos 0 0)),
                         document = doc }

readKeystrokesBlocking :: IO [Char]
readKeystrokesBlocking = do c <- hGetChar stdin
                            rest <- readKeystrokesNonBlocking
                            return $ c : rest
readKeystrokesNonBlocking :: IO [Char]
readKeystrokesNonBlocking = do
  ready <- hReady stdin
  case ready of True -> readKeystrokesBlocking
                False -> return []
readKeystrokes :: IO [Char]
readKeystrokes = if busyLoop then readKeystrokesNonBlocking else readKeystrokesBlocking

{-
readKeystrokes :: IO [Char]
readKeystrokes = do
  k <- readKeystroke
  case k of Just c -> do rest <- readKeystrokes
                         return $ c : rest
            Nothing -> return []
-}

-- Hstrip text (start, lenth) (x, y)
data Hstrip = Hstrip Text (Int, Int) (Int, Int)

render fb (EditorState { viewState = (ViewState vp@(ViewPos vx vy) (CursorPos cx cy)),
                         document = doc }) = do
  --clearScreen
  setCursorPosition 0 0
  B.hPutBuilder stdout $ renderDocument fb doc vp
  setCursorPosition (cy - vy) (cx - vx)
  hFlush stdout

moveCursor (EditorState { viewState = (ViewState (ViewPos vx vy) (CursorPos cx cy)) }) = do setCursorPosition (cy - vy) (cx - vx)
                                                                                            hFlush stdout

data Command = Dir Int Int | Insert Char | Huh String deriving (Eq, Show)

--ctrl c = chr (1 + (ord c - (ord 'a')))

keystrokeToCommand :: Char -> Command
keystrokeToCommand 'h' = Dir (-1) 0
keystrokeToCommand 'l' = Dir 1 0
keystrokeToCommand 'j' = Dir 0 1
keystrokeToCommand 'k' = Dir 0 (-1)
keystrokeToCommand '\006' = Dir 0 10
keystrokeToCommand '\002' = Dir 0 (-10)
keystrokeToCommand 'a' = Insert 'a'
keystrokeToCommand 'b' = Insert 'b'
keystrokeToCommand c = Huh [c]

getEditorState :: State EditorState EditorState
getEditorState = state $ \es -> (es, es)
setEditorState :: EditorState -> State EditorState ()
setEditorState es' = state $ \es -> ((), es')

viewPosFollowCursor fb@(FrameBuffer (w, h)) cp@(CursorPos cx cy) vp@(ViewPos vx vy) = ViewPos nvx nvy
  where nvx = clip vx (cx - w + 1) (cx + 1)
        nvy = clip vy (cy - h + 1) (cy + 1)

moveAndClipCursor doc (CursorPos cx cy) (Dir dx dy) =
  let (newx, newy) = clipCursorToDocument doc (cx + dx, cy + dy)
   in CursorPos newx newy

processCommand :: FrameBuffer -> Command -> State EditorState ()
processCommand fb d@(Dir dx dy) = do
  es@(EditorState { viewState = (ViewState vp cp), document = doc }) <- getEditorState
  let newCp = moveAndClipCursor doc cp d
  let newVp = viewPosFollowCursor fb newCp vp
  setEditorState $ es { viewState = (ViewState newVp newCp) }
processCommand fb (Insert c) = do
  es@(EditorState { viewState = vs@(ViewState vp (CursorPos cx cy)), document = doc }) <- getEditorState
  let newDoc = insertCharInDoc doc cx cy c
  setEditorState $ es { document = newDoc }
processCommand fb (Huh c) = return (leesp "higgs" (show ("huh", c)) ())

clipToFB (FrameBuffer (w, h)) (ViewPos x y) = ViewPos (clip x 0 w) (clip y 0 h)
clip x lo hi
  | x < lo = lo
  | x >= hi-1 = hi-1
  | otherwise = x

processKeys :: FrameBuffer -> [Char] -> State EditorState ()
processKeys fb keys = mapM_ (\key -> processCommand fb (lesp "higgs" (keystrokeToCommand key))) keys

processKeysReturnState es fb keys = case (runState (processKeys fb keys) es) of ((), es') -> es'

data Redraw = Full | MoveCursor | None deriving Show

whatRedrawIsNeeded (EditorState { viewState = (ViewState ovp ocp), document = odoc }) (EditorState { viewState = (ViewState nvp ncp), document = ndoc })
  | odoc /= ndoc = Full
  | ovp /= nvp = Full
  | ocp /= ncp = MoveCursor
  | otherwise = None

editorLoop :: EditorState -> FrameBuffer -> Bool -> IO ()
editorLoop es fb firstTime = do
  if firstTime then render fb es else return ()
  --msp "loop"
  keystrokes <- readKeystrokes
  --let keystrokes = [] :: [Char]
  () <- if ((length keystrokes) == 0) then (return ()) else (debug fb keystrokes)
  let es' = processKeysReturnState es fb keystrokes
  let redraw = if firstTime then Full else whatRedrawIsNeeded es es'
  case redraw of Full -> render fb es'
                 MoveCursor -> moveCursor es'
                 None -> return ()
  editorLoop es' fb False

editorLoopStart es = do
  fb <- getFrameBuffer
  editorLoop es fb True

data Foo = Foo { a :: Int, b :: String } deriving Show

_main = do
  hSetBuffering stdin NoBuffering
  --hSetBuffering stdout NoBuffering
  setSGR [Reset]
  es <- readFileInitState "sample.txt"
  {-
  let doc = document es
   in do msp $ docToListAt doc (0, 0)
         msp $ length $ docToListAt doc (0, 0)
  error ""
  -}
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

main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout (BlockBuffering Nothing)
  (FrameBuffer (wid, ht)) <- getFrameBuffer
  t <- IO.readFile "uni.txt"
  let noSpaces = L.filter ('\n' /=) $ L.filter (' ' /=) $ T.unpack t
  let fillScreenString i = take (wid * (ht-0) - 0) (drop i (cycle noSpaces))
  let fillScreenText i = T.pack $ fillScreenString i
  --let charAt x y = fillScreenString !! (x + y * wid)
  --let row y = V.generate wid (\x -> charAt x y)
{-
  let stringToV2 :: String -> V.Vector (V.Vector Char)
      stringToV2 s = V.generate ht row
        where row y = V.generate wid (\x -> charAt x y)
              charAt x y = s !! (x + y * wid)
      fillScreenV2 = stringToV2 fillScreenString
      rah i = stringToV2 (drop i (cycle fillScreenString))
      fillScreenV2s = map rah [0..99]
-}
  --time "putStr String" $ mapM_ (foo fillScreen) [0..99]
  wsResult <- timeList "writeStrngs" (map writeString (map fillScreenString [0..99])) 1
  wsResult2 <- timeList "writeStrngs2" (map writeString (map fillScreenString [0..99])) 1
  let verp = (map fillScreenString [0..99])
  wsResult3 <- timeList "writeStrngs2" (map writeString verp) 1
  wsResult4 <- timeList "writeStrngs2" (map writeString verp) 1
  wscResult <- timeList "writeStringConvert" (map writeStringConvert (map fillScreenString [0..99])) 1
  wscResult2 <- timeList "writeStringConvert" (map writeStringConvert (map fillScreenString [0..99])) 1
  let verp2 = (map fillScreenString [0..99])
  wscResult3 <- timeList "writeStringConvert" (map writeStringConvert verp2) 1
  wscResult4 <- timeList "writeStringConvert" (map writeStringConvert verp2) 1
  --wtResult <- timeN "writeText" (writeText fillScreenText) 1000
  --wv2Result <- timeN "writeV2" (writeV2 fillScreenV2) 1000
  --wv2cResult <- timeN "writeV2Convert" (writeV2Convert fillScreenV2) 1000
  --wv2cmResult <- timeN "writeV2ConvertMany" (writeV2ConvertMany fillScreenV2s) 1
  --wv2cmResult2 <- timeN "writeV2ConvertMany2" (writeV2ConvertMany fillScreenV2s) 1
  --timeN "writeString" (writeString fillScreenString) 1000
  --timeN "writeText" (writeText fillScreenText) 1000
  msp wsResult
  msp wsResult2
  msp wsResult3
  msp wsResult4
  msp wscResult
  msp wscResult2
  msp wscResult3
  msp wscResult4
  --msp wtResult
  --msp wv2Result
  --msp wv2cResult
  --msp wv2cmResult
  --msp wv2cmResult2
  --threadDelay $ 101 * 1000000
  where writeString s = do
          --clearScreen
          setCursorPosition 0 0
          timing <- time "put string" $ putStr s
          hFlush stdout
          msp timing
        writeStringConvert s = do
          --clearScreen
          setCursorPosition 0 0
          timing <- time "put text" $ IO.hPutStr stdout $ T.pack s
          hFlush stdout
          msp timing
        writeText t = do
          clearScreen
          setCursorPosition 0 0
          IO.hPutStr stdout t
          hFlush stdout
        writeV2 v2 = do
          clearScreen
          setCursorPosition 0 0
          mapM_ writeV v2
          hFlush stdout
        writeV v = do
          mapM_ (hPutChar stdout) v
        writeV2Convert v2 = do
          clearScreen
          setCursorPosition 0 0
          IO.hPutStr stdout (convertV2Text v2)
          hFlush stdout
        writeV2ConvertMany v2s = do
          mapM_ writeV2Convert v2s
        convertV2Text :: V.Vector (V.Vector Char) -> Text
        convertV2Text v2 = TL.toStrict $ TB.toLazyText $ V.foldl (\a v -> (a <> (convertVText v)))  mempty v2
        convertVText :: V.Vector Char -> TB.Builder
-- foldl :: (a -> b -> a) -> a -> Vector b -> a
        convertVText v = V.foldl (\a b -> (a <> (TB.singleton b))) mempty v
