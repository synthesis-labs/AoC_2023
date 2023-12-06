module Day18 where

import           AoC
import           Control.Monad       (join)
import           Control.Monad.State (State, get, put, runState)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import qualified Data.Set            as Set
import           Debug.Trace         (trace)
import           Handy               (WhichPuzzleInput (..), get_puzzle_input,
                                      unique)
import           Parsing             (run_parser, run_parser_with_state)
import           Text.Parsec         (Parsec, anyChar, char, choice, digit,
                                      getState, letter, many1, newline,
                                      optional, sepBy, setState, string, try,
                                      (<|>))

parse_it :: Parsec String () ()
parse_it = do
  pure ()

solve1 :: () -> Int
solve1 _ = 0

solve2 :: () -> Int
solve2 _ = 0

solve :: IO (Solution Int)
solve = do
  input <- pure () -- run_parser parse_it <$> get_puzzle_input Mine 2023 18
  let solution_1 = solve1 input
  let solution_2 = solve2 input
  pure $ SolvedTwo 2023 18 solution_1 (Unknown) solution_2 (Unknown)
