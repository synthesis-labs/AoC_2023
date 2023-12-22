module Day04 where
import Data.List (intersect)
import Utils (captureGroups)

parseNumbers :: String -> [Int]
parseNumbers = map read . words

findWinningNumbers :: String -> [Int]
findWinningNumbers card = winningNumbers `intersect` myNumbers
  where
    [winningNumberStr, myNumberStr] = captureGroups card ".*: (.*)\\|(.*)"
    (winningNumbers, myNumbers) = (parseNumbers winningNumberStr, parseNumbers myNumberStr)

solveOne :: String -> Int
solveOne line = if null nrs then 0 else 2 ^ (length nrs - 1)
  where
    nrs = findWinningNumbers line

countOccurrences :: Int -> [Int] -> Int
countOccurrences x xs = length (filter (==x) xs)

cardNumber :: String -> Int
cardNumber card = read $ head $ captureGroups card "Card\\s+(\\d+)"

count' :: String -> [Int]
count' card = [cardNumber card + 1..cardNumber card + length (findWinningNumbers card)]

count'' :: [Int] -> String -> [Int]
count'' collected card = cardNumber card : concat (replicate n (count' card))
  where
    n = 1 + countOccurrences (cardNumber card) collected

collectCards :: [String] -> [Int] -> [Int]
collectCards [card] collected = count'' collected card ++ collected
collectCards (card:cards) collected = collectCards cards (count'' collected card ++ collected)

run :: IO ()
run = do
  contents <- lines <$> readFile "./data/day04"
  let answer1 = sum [solveOne x | x <- contents]
  let answer2 = length (collectCards contents [])
  putStrLn $ "Answer Q1 => " ++ show answer1
  putStrLn $ "Answer Q2 => " ++ show answer2