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

fillScreen = do
  Just (h, w) <- getTerminalSize
  drawBox (0, 0) (box (w-0) (h-1) 'x')
  drawBox (0, (h-1)) (box (w-0) 1 'y')

speedTest = time "speedTest" $ mapM_ (\_ -> fillScreen) [0..99]

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

data Document = Document (V.Vector Text) deriving Show

readFileAsDoc :: String -> IO Document
readFileAsDoc filename = do
  entireFile <- readFile filename
  let foo :: [Text]
      foo = T.splitOn "\n" (T.pack entireFile)
  return $ Document $ V.fromList $ T.splitOn "\n" (T.pack entireFile)

-- Position of the cursor relative to the screen origin
data CursorPos = CursorPos Int Int
-- Position of the text character at the screen origin
data ViewPos = ViewPos Int Int

data ViewState = ViewState ViewPos CursorPos

type Generation = Int

-- Int: generation
data EditorState = EditorState Generation ViewState Document

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

debug s = do
  Just (h, w) <- getTerminalSize
  setCursorPosition (h-1) 0
  putStr s

renderDocument (ViewPos vx vy) (Document lines) = do
  mapM_ drawRow (zip [0..] (drop vy (V.toList lines)))
  where drawRow (y, row) = do setCursorPosition y 0
                              putStr (drop vx (unpack row))

render (EditorState _ (ViewState vp cp) doc) = do
  clearScreen
  renderDocument vp doc

updateTerminalSize (w, h) = do
  ts <- getTerminalSize
  return $ case ts of Just (h, w) -> (w, h)
                      Nothing -> (w, h)

updateEditorState es keystrokes = es

editorLoop es generation (w, h) = do
  --msp "loop"
  --(w, h) <- updateTerminalSize (w, h)
  keystrokes <- readKeystrokes
  --let keystrokes = [] :: [Char]
  () <- if ((length keystrokes) == 0) then (return ()) else (debug keystrokes)
  let es' = updateEditorState es keystrokes
      needsRedraw = case generation of Just oldGeneration -> oldGeneration /= (generationOf es')
                                       Nothing -> True
  --msp needsRedraw
  () <- if needsRedraw then render es' else return ()
  editorLoop es' (Just $ generationOf es') (w, h)

editorLoopStart es = do
  Just (h, w) <- getTerminalSize
  editorLoop es Nothing (w, h)

main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  setSGR [Reset]
  es <- readFileInitState "sample.txt"
  withRawInput 0 1 $ editorLoopStart es
  putStrLn "done2"
  msp "done"

push :: Int -> State [Int] ()
push x = state (\xs -> ((), x:xs))
pop :: State [Int] Int
pop = state (\(x:xs) -> (x, xs))
blah = do
  push 3
  a <- pop
  return a

_main = msp $ runState blah [2]
