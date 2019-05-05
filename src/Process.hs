module Process
( attachProcess
) where

import Control.Concurrent
import Control.Concurrent.Chan

import EditorState
import Event
import Util
import Types

attachProcess :: String -> Chan Event -> (String -> String) -> IO ()
attachProcess bufferName chan contentsTransformer = do
  let loop :: IO ()
      loop = do
        threadDelay 250000
        writeChan chan $ RequestTransform bufferName contentsTransformer
        loop
  forkIO loop
  return ()
