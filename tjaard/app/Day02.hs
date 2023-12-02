module Day02 where

import Data.List.Split (splitOn)
import Data.List (isInfixOf)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)

-- Data model
reds, greens, blues :: Int; reds = 12; greens = 13; blues = 14
data GameSet = GameSet { red :: Int, green :: Int, blue :: Int }
data Game = Game { gameNumber :: Int, sets :: [GameSet], possible :: Bool, power :: Int }

-- Parsers for our data model
-- Parse a single set: "red: 1, green: 4, blue: 2"
parseGameSet :: String -> GameSet
parseGameSet setStr =
    GameSet { red = count' "red" setStr, green = count' "green" setStr, blue = count' "blue" setStr }

-- Parse an entire game string
parseGame :: String -> Game
parseGame str =
    let
        [gameNumberStr, setStr] = splitOn ":" str
        setStrs = splitOn ";" setStr
        gameNumber = read (last (words gameNumberStr))
        sets = map parseGameSet setStrs
    in Game { gameNumber = gameNumber, sets = sets, possible = possible' sets, power = power' sets }

-- Helper functions
count' :: String -> String -> Int
count' color colorStr =
    let
        colorMatch = fromMaybe "0 _" (find (\c -> color `isInfixOf` c) (splitOn "," colorStr))
        [colorCount, _color] = words colorMatch
    in
        read colorCount

possible' :: [GameSet] -> Bool
possible' = all (\set -> red set <= reds && green set <= greens && blue set <= blues)

power' :: [GameSet] -> Int
power' sets = maximum (map red sets) * maximum (map green sets) * maximum (map blue sets)

-- Main run
run :: IO ()
run = do
  contents <- lines <$> readFile "./data/day02-test"
  let games = [parseGame x | x <- contents]
  let possibleSum = sum (map gameNumber (filter possible games))
  let powerSum = sum (map power games)

  -- Print each game if you'd like...
  --mapM_ putStrLn [showGame x | x <- games]

  putStrLn $ "Answer Q1 => " ++ show possibleSum
  putStrLn $ "Answer Q2 => " ++ show powerSum