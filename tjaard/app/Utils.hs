module Utils where
import Data.List (isInfixOf, isPrefixOf)

halve :: String -> (String, String)
halve s = splitAt (length s `div` 2) s

findSubstringIndex :: String -> String -> Maybe Int
findSubstringIndex substring str
    | substring `isInfixOf` str = Just (length $ takeWhile (/= substring) $ iterate tail str)
    | otherwise = Nothing

startsWith :: String -> String -> Bool
startsWith prefix str = prefix `isPrefixOf` str