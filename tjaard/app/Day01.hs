module Day01 where

import Data.Char (isDigit)
import Data.List (elemIndex)
import Utils (startsWith)
import Data.Maybe (fromMaybe)

digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
reversedDigits = map reverse digits

getFirstDigit :: String -> [String] -> String
getFirstDigit [] ds = ""
getFirstDigit str@(x:xs) ds
  | isDigit x = [x]
  | or [startsWith digit str | digit <- ds] = show (1 + fromMaybe 0 (elemIndex True [startsWith digit str | digit <- ds]))
  | otherwise = getFirstDigit xs ds

solveOne :: String -> Int
solveOne s = read (getFirstDigit s digits ++ getFirstDigit (reverse s) reversedDigits)

run :: IO ()
run = do
  contents1 <- lines <$> readFile "./data/day01-test"
  contents2 <- lines <$> readFile "./data/day01-test2"
  let answer = sum [solveOne x | x <- contents1]
  let answer2 = sum [solveOne x | x <- contents2]
  putStrLn $ "Answer Q1 => " ++ show answer
  putStrLn $ "Answer Q2 => " ++ show answer2