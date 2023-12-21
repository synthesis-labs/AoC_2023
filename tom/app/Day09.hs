module Day09 where

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
                                      optionMaybe, optional, sepBy, setState,
                                      string, try, (<|>))

parse_sequence :: Parsec String () [Int]
parse_sequence =
  let parse_value = do
        negative <- optionMaybe (char '-')
        val <- read <$> many1 digit
        pure $ maybe val (const $ val * (-1)) negative
   in (parse_value `sepBy` char ' ') <* newline

make_deltas :: [Int] -> [Int] -> [Int]
make_deltas (a:b:rest) acc = make_deltas (b : rest) ((b - a) : acc)
make_deltas _ acc          = reverse acc -- reverse because of cons ^^ and tail rec

solve1 :: [[Int]] -> Int
solve1 input =
  let next :: [Int] -> Int
      next i =
        let deltas = make_deltas i []
         in if all ((==) 0) deltas
              then last i
              else last i + (next deltas)
   in sum $ next <$> input

solve2 :: [[Int]] -> Int
solve2 input =
  let prev :: [Int] -> Int
      prev i =
        let deltas = make_deltas i []
         in if all ((==) 0) deltas
              then head i
              else head i - (prev deltas)
   in sum $ prev <$> input

solve :: IO (Solution Int)
solve = do
  input <- run_parser (many1 parse_sequence) <$> get_puzzle_input Mine 2023 9
  solution_1 <- pure $ solve1 input
  solution_2 <- pure $ solve2 input
  pure $
    SolvedTwo 2023 9 solution_1 (Revealed 1916822650) solution_2 (Revealed 966)
