{-# LANGUAGE BlockArguments #-}

module SpeedTests ( speedTests ) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally, catch, IOException)
import Control.Monad.State
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
import qualified Data.Text.Foreign as TF (peekCStringLen)
import qualified Data.Text.IO as IO
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Vector (Vector, (!))
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FT
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff)
import System.Console.ANSI
import System.IO
import System.Posix.IO (fdRead, stdInput)
import System.Posix.Terminal
import System.Random

import Util

rectSize = (8, 3)
topBot = "+" ++ (take ((fst rectSize) - 2) (repeat '-')) ++ "+"
leftRight = "|" ++ (take ((fst rectSize) - 2) (repeat '.')) ++ "|"
rect :: [String]
rect = [topBot] ++ mids ++ [topBot]
  where mids = take ((snd rectSize) - 2) $ repeat leftRight
numRects = 100
data Rect = Rect (Int, Int) (Int, Int) deriving Show

rectangles :: IO ()
rectangles = do
  Just (ht, wid) <- getTerminalSize
  msp ("ah", wid, ht)
  rects <- makeRects wid ht
  msp rects
  let dt = 0.01
  --clearScreen
  let loop t = do
        clearScreen
        mapM (drawRect t) rects
        hFlush stdout
        threadDelay 20000
        loop (t + dt)
        where drawRect t (Rect (x0, y0) (x1, y1)) = drawRectAt x y
                where x = floor $ (fromIntegral x0) + ((fromIntegral (x1 - x0)) * ct)
                      y = floor $ (fromIntegral y0) + ((fromIntegral (y1 - y0)) * ct)
                      ct = t - (fromIntegral (floor t))
  loop 0

drawRectAt :: Int -> Int -> IO ()
drawRectAt x y =
  let drawRectLine :: (Int, String) -> IO ()
      drawRectLine (i, s) = do
        setCursorPosition (y+i) x
        --IO.hPutStr stdout s
        putStr s
        --msp ("ah", x, y, i)
   in mapM_ drawRectLine (zip [0..] rect)

makeRects :: Int -> Int -> IO [Rect]
makeRects wid ht = mapM (const (makeRect wid ht)) [0..numRects-1]
makeRect :: Int -> Int -> IO Rect
makeRect wid ht = do
  a <- randPoint (wid - (fst rectSize) - 1) (ht - (snd rectSize) - 1)
  b <- randPoint (wid - (fst rectSize) - 1) (ht - (snd rectSize) - 1)
  return $ Rect a b

randPoint :: Int -> Int -> IO (Int, Int)
randPoint maxX maxY = do
  x <- getStdRandom (randomR (0, maxX))
  y <- getStdRandom (randomR (0, maxY))
  return (x, y)

-- String -> CString -> Ptr CChar, then pack it into a bytestring and write it

data QRect = QRect FT.CChar (Int, Int) (Int, Int)
makeQRect = do
  c <- rnd 65 (97+25)
  x <- rnd 50 150
  y <- rnd 15 45
  dx <- rnd (-1) 1
  dy <- rnd (-1) 1
  return $ QRect c (x, y) (dx, dy)

rnd lo hi = getStdRandom (randomR (lo, hi))
makeQRects n = mapM (const makeQRect) [0..n-1]

drawIntoStringSpeedTests = do
  Just (ht, wid) <- getTerminalSize
  let fillScreenString = take (wid * (ht-0) - 0) (repeat '.')
      --fillScreenText i = T.pack $ fillScreenString i
  FCS.withCString fillScreenString (dISS2 wid ht (length fillScreenString))
  -- setCursorPosition 0 0
  -- timing <- time "put text" $ IO.hPutStr stdout $ T.pack s
  -- hFlush stdout
  -- msp timing

nsy = [0..9]

drawBox :: FT.CChar -> Int -> Int -> Int -> FCS.CString -> IO ()
drawBox c x y wid buf = do
  let tl = x + (y * wid)
      boxHeight = 3
  flip mapM [0..boxHeight-1] $ \y -> do
    flip mapM nsy $ \x -> do
      let off = tl + x + (wid * y)
      pokeElemOff buf off c
  return ()

drawBox' :: QRect -> Int -> Int -> FCS.CString -> IO ()
drawBox' (QRect c (x, y) (dx, dy)) t wid buf = do
  drawBox c x' y' wid buf
  return ()
  where x' = x + ((t * dx) `mod` 40)
        y' = y + ((t * dy) `mod` 10)

clear cString len = do
  flip mapM [0..len-1] $ \offset -> pokeElemOff cString offset 32

drawBoxes :: [QRect] -> Int -> Int -> FCS.CString -> Int -> IO ()
drawBoxes boxes t wid cString len = do
  clear cString len
  mapM (\r -> drawBox' r t wid cString) boxes
  --flip mapM [0..99] $ const $ drawBox wid cString
  let cl = (cString, len)
  t <- TF.peekCStringLen cl
  setCursorPosition 0 0
  IO.putStr t
  hFlush stdout

dISS2 :: Int -> Int -> Int -> FCS.CString -> IO ()
dISS2 wid ht len cString = do
  clearScreen
  -- x <- peek cString
  -- x' <- peekElemOff cString 0
  -- x'' <- peekElemOff cString 1
  -- msp x
  -- msp x'
  -- msp x''
  -- let cl = (cString, len)
  -- t <- TF.peekCStringLen cl
  -- IO.putStr t
  -- msp "ho"
  -- msp "ho"
  -- pokeElemOff cString 1 116
  -- t' <- TF.peekCStringLen cl
  -- IO.putStr t'
  rects <- makeQRects 120
  --report <- timeN "screen" (drawBoxes wid cString len) 1000
  let loop t = do
        drawBoxes rects t wid cString len
        setCursorPosition 0 0
        msp t
        hFlush stdout
        threadDelay 5000
        if t < 4000000
           then loop (t+1)
           else return ()
  loop 0
  -- setCursorPosition 0 0
  -- putStrLn report
  -- hFlush stdout
  setCursorPosition 0 0
  msp ("scrn", wid, ht)
  hFlush stdout
  threadDelay 10000000

speedTests = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout (BlockBuffering Nothing)
  --writeStringSpeedTests
  --rectangles
  drawIntoStringSpeedTests
  msp "sthi"

writeStringSpeedTests = do
  Just (wid, ht) <- getTerminalSize
  t <- IO.readFile "uni.txt"
  let noSpaces = L.filter ('\n' /=) $ L.filter (' ' /=) $ T.unpack t
      fillScreenString i = take (wid * (ht-0) - 0) (drop i (cycle noSpaces))
      fillScreenText i = T.pack $ fillScreenString i
      numTrials = 20 -- 99
  --let charAt x y = fillScreenString !! (x + y * wid)
  --let row y = V.generate wid (\x -> charAt x y)
{-
  let stringToV2 :: String -> V.Vector (V.Vector Char)
      stringToV2 s = V.generate ht row
        where row y = V.generate wid (\x -> charAt x y)
              charAt x y = s !! (x + y * wid)
      fillScreenV2 = stringToV2 fillScreenString
      rah i = stringToV2 (drop i (cycle fillScreenString))
      fillScreenV2s = map rah [0..numTrials]
-}
  --time "putStr String" $ mapM_ (foo fillScreen) [0..numTrials]
  wsResult <- timeList "writeStrngs" (map writeString (map fillScreenString [0..numTrials])) 1
  wsResult2 <- timeList "writeStrngs2" (map writeString (map fillScreenString [0..numTrials])) 1
  let verp = (map fillScreenString [0..numTrials])
  wsResult3 <- timeList "writeStrngs2" (map writeString verp) 1
  wsResult4 <- timeList "writeStrngs2" (map writeString verp) 1
  wscResult <- timeList "writeStringConvert" (map writeStringConvert (map fillScreenString [0..numTrials])) 1
  wscResult2 <- timeList "writeStringConvert" (map writeStringConvert (map fillScreenString [0..numTrials])) 1
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
