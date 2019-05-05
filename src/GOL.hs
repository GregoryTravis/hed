module GOL
( gol ) where

import Data.List (intercalate)
import Data.List.Split
import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Bool)

gol :: String -> String
gol = gridToString . gol' . stringToGrid
gol' :: Grid -> Grid
gol' g = gridMap g huh
  where huh :: (Int, Int) -> Bool
        huh (x, y) = rule numLive
          where 
            numLive :: Int
            numLive = length $ filter id around
            around :: [Bool]
            around = map (sampleDelta g) pointsAround
            sampleDelta :: Grid -> (Int, Int) -> Bool
            sampleDelta g (dx, dy) = sample g (x+dx, y+dy)
            rule :: Int -> Bool
            rule n | n == 3 = True
                   | n == 2 = sample g (x, y)
                   | otherwise = False

-- Any live cell with fewer than two live neighbours dies (referred to as underpopulation or exposure[1]).
-- Any live cell with more than three live neighbours dies (referred to as overpopulation or overcrowding).
-- Any live cell with two or three live neighbours lives, unchanged, to the next generation.
-- Any dead cell with exactly three live neighbours will come to life.

pointsAround :: [(Int, Int)]
pointsAround = [(x, y) | x <- [-1..1], y <- [-1..1], x /= y]

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

-- Return grid contents at the given point, or False if it's outside the grid.
sample :: Grid -> (Int, Int) -> Bool
sample g (x, y)
  | outside g x y = False
  | otherwise = (g V.! y) V.! x
  where outside g x y = x < 0 || x >= V.length (g V.! 0) || y < 0 || y >= V.length g

gridMap :: Grid -> ((Int, Int) -> Bool) -> Grid
gridMap g f = rows
  where --points :: [(Int, Int)]
        --points = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
        row :: Int -> V.Vector Bool
        row y = V.fromList $ map (\x -> f (x, y)) [0..w-1]
        rows = V.fromList $ map row [0..h-1]
        w = V.length (g V.! 0)
        h = V.length g
