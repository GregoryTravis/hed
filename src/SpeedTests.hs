module SpeedTests ( speedTests ) where

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

import Util

speedTests = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout (BlockBuffering Nothing)
  Just (wid, ht) <- getTerminalSize
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
