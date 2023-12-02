module Utils where
import Data.List (isInfixOf, isPrefixOf)
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