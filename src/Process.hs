module Process
( attachProcess
) where

import Control.Concurrent
import Control.Concurrent.Chan

import EditorState
import Event
import Util
import Types

attachProcess :: String -> Chan Event -> (String -> String) -> Double -> IO ()
attachProcess bufferName chan contentsTransformer speed = do
  let loop :: IO ()
      loop = do
        threadDelay $ floor (1000000.0 / speed)
        writeChan chan $ RequestTransform bufferName contentsTransformer
        loop
  forkIO loop
  return ()
