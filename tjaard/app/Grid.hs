{-# LANGUAGE NamedFieldPuns #-}

module Grid where
import Data.List (find)
import Data.Maybe (catMaybes)

{- USAGE:
grid = parseGrid lines
print $ showGrid grid
maybe (print "N/A") (print . showCoordinate) (at' (1, 1) grid)
-}

data Coordinate = Coordinate {x :: Int, y :: Int, val :: Char}
newC :: Int -> Int -> Char -> Coordinate
newC = Coordinate

type Grid = [Coordinate]

width :: Grid -> Int
width = maximum . map x

height :: Grid -> Int
height = maximum . map y

at' :: (Int, Int) -> Grid -> Maybe Coordinate
at' (x', y') grid = find (\c -> x c == x' && y c == y') grid

getSurroundings :: Grid -> Coordinate -> [Coordinate]
getSurroundings grid (Coordinate x y _) = catMaybes [at' (x', y') grid | x' <- [x-1..x+1], y' <- [y-1..y+1], (x', y') /= (x, y), x' >= 0, y' >= 0, y' < height grid, x' < width grid]

parseLine :: String -> Int -> Int -> [Coordinate] -> [Coordinate]
parseLine [i] x y cs = cs ++ [Coordinate {x, y, val = i}]
parseLine (i : is) x y cs = parseLine is x (y + 1) (cs ++ [Coordinate {x, y, val = i}])

parseGrid :: [String] -> Grid
parseGrid lines = concat $ zipWith parseLine' [0..] lines
  where
    parseLine' :: Int -> String -> [Coordinate]
    parseLine' x line = parseLine line x 0 []

showCoordinate :: Coordinate -> String
showCoordinate c = "(" ++ show (x c) ++ "," ++ show (y c) ++ "):" ++ [val c]

showGrid :: Grid -> String
showGrid g = unwords [showCoordinate c | c <- g]