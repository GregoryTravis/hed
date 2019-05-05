module GOL
( gol ) where

import Data.List (intercalate)
import Data.List.Split
import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Bool)

gol :: String -> String
gol = gridToString . gol' . stringToGrid
gol' :: Grid -> Grid
gol' g = V.map (V.map not) g

charToBool '.' = False
charToBool 'o' = True
boolToChar False = '.'
boolToChar True = 'o'

stringToGrid :: String -> Grid
stringToGrid s = V.fromList $ map V.fromList (map (map charToBool) lines)
  where lines = splitOn "\n" s

gridToString :: Grid -> String
gridToString g = join $ map (map boolToChar) $ map V.toList $ V.toList g
  where join = intercalate "\n"
