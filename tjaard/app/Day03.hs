module Day03 where

import Data.Char (isDigit)
import Utils (captureGroups, extractNumbersWithIndex)

-- I've created a generic grid data model, with a list of coordinates (x,y) and their values
import Grid

-- Coordinate (x,y) of a number. E.g. 114 @ (0,6)
data NumberCoordinate = NumberCoordinate {nr :: Int, coordinate :: Coordinate}
newNC :: Int -> Int -> Int -> NumberCoordinate
newNC nr x y = NumberCoordinate { nr = nr, coordinate = newC x y '_' }

-- Find the coordinates of all numbers
getNumberCoordinates :: [String] -> [NumberCoordinate]
getNumberCoordinates lines = concat $ zipWith nrs [0 ..] lines
  where
    nrs :: Int -> String -> [NumberCoordinate]
    nrs x' line = [newNC nr' x' y' | (nr', y') <- either (const []) id (extractNumbersWithIndex line)]

-- Find coordinates (x,y) of all gears (*)
getGearCoordinates :: Grid -> [Coordinate]
getGearCoordinates grid = filter (\c -> val c == '*') grid

-- Is there a symbol in this list of coordinates?
containsSymbol :: [Coordinate] -> Bool
containsSymbol cs = any (\c -> val c /= '.' && not (isDigit (val c))) cs

-- Get the surroundings of a number, calling getSurroundings for the length of the number
getNumberSurroundings :: Grid -> NumberCoordinate -> [Coordinate]
getNumberSurroundings grid (NumberCoordinate nr c) = concat [getSurroundings grid (newC (x c) y' '_') | y' <- [y c .. y c + length (show nr) - 1]]

-- Get number coordinates surrounding a coordinate
getSurroundingNCs :: [NumberCoordinate] -> Coordinate -> [NumberCoordinate]
getSurroundingNCs ncs (Coordinate cx cy _) = filter (\(NumberCoordinate nr (Coordinate ncX ncY _)) -> ncX >= cx-1 && ncX <= cx+1 && ncY >= cy - length (show nr) && ncY <= cy + 1) ncs

-- If there are two surrounding numbers, multiply them for the gear ratio
getGearRatio :: Grid -> [NumberCoordinate] -> Coordinate -> Int
getGearRatio grid ncs c =
  if length surroundingNumbers == 2
    then surroundingNumbers !! 0 * surroundingNumbers !! 1
    else 0
  where
    surroundingNumbers = map nr $ getSurroundingNCs ncs c

run :: IO ()
run = do
  contents <- lines <$> readFile "./data/day03"
  let grid = parseGrid contents
  let ncs = getNumberCoordinates contents
  let gears = getGearCoordinates grid

  let answer1 = sum . map nr $ filter (containsSymbol . getNumberSurroundings grid) ncs
  let answer2 = sum $ map (getGearRatio grid ncs) gears
  putStrLn $ "Answer Q1 => " ++ show answer1
  putStrLn $ "Answer Q2 => " ++ show answer2