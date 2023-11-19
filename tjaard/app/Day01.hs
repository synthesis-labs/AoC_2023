module Day01 where

import Data.List (sort)

-- Some aliases to make the recursive function clearer
type InputLines = [String]
type CurrentSum = Int
type ElfSums = [Int]

sumUntilEmptyLine :: InputLines -> CurrentSum -> ElfSums -> ElfSums
sumUntilEmptyLine [i] cs es = read i + cs : es -- Last element of the list, sum and return appended to elf sums
sumUntilEmptyLine ("":is) cs es = sumUntilEmptyLine is 0 (cs : es) -- We've reached an empty line, append current sum and restart at 0
sumUntilEmptyLine (i:is) cs es = sumUntilEmptyLine is (read i + cs) es -- Sum the current number, append and recurse

run :: IO ()
run = do
  contents <- lines <$> readFile "./data/day01" -- Use day01-test for testing
  let elfSums = sumUntilEmptyLine contents 0 []
  putStrLn $ "Top Elf calories: " ++ show (maximum elfSums)
  putStrLn $ "Top three Elf calories: " ++ show (sum $ take 3 $ reverse (sort elfSums))