module Utils where

import Control.Monad (void)
import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Regex.PCRE

halve :: String -> (String, String)
halve s = splitAt (length s `div` 2) s

findSubstringIndex :: String -> String -> Maybe Int
findSubstringIndex substring str
  | substring `isInfixOf` str = Just (length $ takeWhile (/= substring) $ iterate tail str)
  | otherwise = Nothing

startsWith :: String -> String -> Bool
startsWith prefix str = prefix `isPrefixOf` str

-- Haskell's way to get capture groups is ugly, I'm wrapping it here
captureGroups :: String -> String -> [String]
captureGroups str pattern =
  captureGroups
  where
    (before, entireMatch, after, captureGroups) = str =~ pattern :: (String, String, String, [String])

numberParser :: Parser (Int, Int)
numberParser = do
  void $ many (satisfy (not . isDigit))  -- Consume any non-digit characters that immediately precede the number
  startIndex <- getPosition
  num <- many1 (satisfy isDigit)
  void $ many (satisfy (not . isDigit))  -- Consume any non-digit characters that immediately follow the number
  return (read num, sourceColumn startIndex - 1)

extractNumbersWithIndex :: String -> Either ParseError [(Int, Int)]
extractNumbersWithIndex input = parse (many numberParser) "" input