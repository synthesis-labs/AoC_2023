module Day01 where

import           AoC
import           Handy (get_puzzle_input)

solve :: IO (Solution String)
solve = do
  putStrLn "Workings of day 1 problems go here... 🚀"
  _ <- get_puzzle_input 2023 1
  pure $ SolvedTwo 2023 1 "dog" (Revealed "dog") "cat" Unknown
