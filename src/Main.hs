{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (finally, catch, IOException)
import Data.Char (ord)
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
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

data Document = Document (V.Vector Text) deriving (Eq, Show)

readFileAsDoc :: String -> IO Document
readFileAsDoc filename = do
  entireFile <- readFile filename
  let foo :: [Text]
      foo = T.splitOn "\n" (T.pack entireFile)
  return $ Document $ V.fromList $ T.splitOn "\n" (T.pack entireFile)

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
  framebufferDrawHstrip fb (Hstrip (T.pack s) (0, w) (0, h-1))

-- Hstrip text (start, lenth) (x, y)
data Hstrip = Hstrip Text (Int, Int) (Int, Int)

-- FrameBuffer (w, h)
data FrameBuffer = FrameBuffer (Int, Int)

getFrameBuffer = do
  Just (h, w) <- getTerminalSize
  return $ FrameBuffer (w, h)

framebufferDrawHstrip :: FrameBuffer -> Hstrip -> IO ()
framebufferDrawHstrip (FrameBuffer (w, h)) (Hstrip text (start, length) (x, y))
  | y < 0 || y >= h = error (show ("oob", w, h, start, length, x, y))
  | x < 0 || x + length > w = error (show ("oob", w, h, start, length, x, y))
  | otherwise = do setCursorPosition y x
                   putStr (take length (drop start (T.unpack text)))

renderDocument fb@(FrameBuffer (w, h)) (ViewPos vx vy) (Document lines) = do
  mapM_ drawRow (zip [0..] (take h (drop vy (V.toList lines))))
  where drawRow (y, text) = do framebufferDrawHstrip fb (Hstrip text (vx, w) (0, y))
                               framebufferDrawHstrip fb (Hstrip blankLine (0, w-(T.length text)) (max 0 (T.length text - vx), y))
        blankLine = T.pack (replicate w ' ')

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

processCommand :: Command -> State EditorState ()
processCommand (Dir dx dy) = do
  es@(EditorState generation (ViewState (ViewPos x y) cp) doc) <- getEditorState
  setEditorState $ EditorState (generation + 1) (ViewState (ViewPos (x + dx) (y + dy)) cp) doc
processCommand (Huh _) = return ()

processKeys :: [Char] -> State EditorState ()
processKeys keys = mapM_ (\key -> processCommand (keystrokeToCommand key)) keys

processKeysReturnState es keys = case (runState (processKeys keys) es) of ((), es') -> es'

editorLoop es fb generation = do
  --msp "loop"
  keystrokes <- readKeystrokes
  --let keystrokes = [] :: [Char]
  () <- if ((length keystrokes) == 0) then (return ()) else (debug fb keystrokes)
  let es' = processKeysReturnState es keystrokes
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
