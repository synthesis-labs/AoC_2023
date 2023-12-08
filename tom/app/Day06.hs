module Day06 where

import           AoC

import           Control.Monad       (join)
import           Control.Monad.State (State, get, put, runState)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import qualified Data.Set            as Set
import           Debug.Trace         (trace)
import           Handy               (SeekResult (..), WhichPuzzleInput (..),
                                      get_puzzle_input, seeker, unique)
import           Numeric.Search
import           Parsing             (run_parser, run_parser_with_state)

import           Text.Parsec         (Parsec, anyChar, char, choice, digit,
                                      getState, letter, many1, newline,
                                      optional, sepBy, setState, string, try,
                                      (<|>))

type TimeDist = (Int, Int)

parse_it :: Parsec String () [TimeDist]
parse_it = do
  _ <- string "Time:" <* (many1 $ char ' ')
  times <- (read <$> many1 digit) `sepBy` (many1 $ char ' ') <* newline
  _ <- string "Distance:" <* (many1 $ char ' ')
  dists <- (read <$> many1 digit) `sepBy` (many1 $ char ' ') <* newline
  pure $ zip times dists

travel :: Int -> Int -> Int
travel held total = (held * (total - held))

solve1 :: [TimeDist] -> Int
solve1 input =
  let results =
        filter ((==) True) <$>
        (\(race_t, record_d) ->
           [record_d < travel h race_t | h <- [1 .. race_t]]) <$>
        input
   in product $ length <$> results

fix_input :: [TimeDist] -> TimeDist
fix_input input =
  let time = read $ join $ (show . fst) <$> input
      dist = read $ join $ (show . snd) <$> input
   in (time, dist)

-- Super slow bruteforce approach
solve2 :: [TimeDist] -> Int
solve2 input = solve1 [fix_input input]

solve :: IO (Solution Int)
solve = do
  input <- run_parser parse_it <$> get_puzzle_input Mine 2023 6
  putStrLn $ show input
  solution_1 <- pure $ solve1 input
  solution_2 <- pure $ solve2 input
  pure $
    SolvedTwo 2023 6 solution_1 (Revealed 633080) solution_2 (Revealed 20048741)
