module SizeReport
( getCharsOrSizeReport
, updateTerminalSize
) where

import Data.Char (isDigit)
import qualified Data.Map as M
import System.Console.ANSI

data ParseState = Esc | LSQB | FirstDigit | SecondDigit | Success | Fail
  deriving (Eq, Ord)
type Recognizer = Char -> ParseState
stateMachine :: M.Map ParseState Recognizer
stateMachine = M.fromList
  [ (Esc, \c -> if c == '\ESC' then LSQB else Fail)
  , (LSQB, \c -> if c == '[' then FirstDigit else Fail)
  , (FirstDigit, \c -> if c >= '0' && c <= '9' then FirstDigit else if c == ';' then SecondDigit else Fail)
  , (SecondDigit, \c -> if c >= '0' && c <= '9' then SecondDigit else if c == 'R' then Success else Fail)
  ]

parseSizeReport :: String -> (Int, Int)
parseSizeReport s = (read second, read first) -- It's (h, w) of course
  where first = takeWhile isDigit $ drop 2 s
        second = takeWhile isDigit $ drop 1 $ dropWhile isDigit $ drop 2 s

getCharsOrSizeReport :: IO (Either (Int, Int) String)
getCharsOrSizeReport = step Esc []
  where step :: ParseState -> String -> IO (Either (Int, Int) String)
        step state sofar = do
          c <- getChar
          case (stateMachine M.! state) c of Success -> return $ Left $ parseSizeReport (sofar ++ [c])
                                             Fail -> return $ Right $ sofar ++ [c]
                                             next -> step next (sofar ++ [c])

updateTerminalSize = do
  saveCursor
  setCursorPosition 999 999
  reportCursorPosition
