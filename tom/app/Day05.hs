module Day05 where

import           AoC
import           Control.Monad        (join)
import           Control.Monad.State  (State, get, put, runState)
import           Data.List            (find)
import           Data.Maybe           (fromMaybe)
import qualified Data.Set             as Set
import           Debug.Trace          (trace)
import           Handy                (WhichPuzzleInput (..), get_puzzle_input,
                                       unique)
import           Numeric.Search.Range (searchFromTo)
import           Parsing              (run_parser, run_parser_with_state)
import           Text.Parsec          (Parsec, anyChar, char, choice, digit,
                                       getState, letter, many1, newline,
                                       optional, sepBy, setState, string, try,
                                       (<|>))

data MapArrow =
  MapArrow String String (Int -> Int)

data Plan =
  Plan [Int] [MapArrow]

parse_range :: Parsec String () (Int, Int, Int)
parse_range = do
  dest_start <- read <$> many1 digit <* char ' '
  source_start <- read <$> many1 digit <* char ' '
  range_length <- read <$> many1 digit
  pure (dest_start, source_start, range_length)

parse_map :: Parsec String () MapArrow
parse_map = do
  from <- many1 letter <* string "-to-"
  to <- many1 letter <* string " map:" <* newline
  ranges <- many1 (parse_range <* newline) <* optional newline
  pure $ MapArrow from to (mk_mapper ranges)

parse_plan :: Parsec String () Plan
parse_plan = do
  seeds <-
    string "seeds: " *> (read <$> many1 digit) `sepBy` char ' ' <* newline
  mappers <- newline *> many1 parse_map
  pure $ Plan seeds mappers

source_within_range :: Int -> (Int, Int, Int) -> Bool
source_within_range source (dst, src, len) = source >= src && source < src + len

mk_mapper :: [(Int, Int, Int)] -> Int -> Int
mk_mapper ranges input =
  case find (source_within_range input) ranges of
    Nothing              -> input
    Just (dst, src, len) -> dst + input - src

solve1 :: Plan -> Int
solve1 (Plan seeds arrows) =
  let mapper = foldl (\acc -> \(MapArrow f t m) -> m . acc) id arrows
   in minimum $ mapper <$> seeds

seeds_as_ranges :: [Int] -> [(Int, Int)]
seeds_as_ranges []         = []
seeds_as_ranges (a:b:rest) = (a, b) : seeds_as_ranges rest

solve2 :: Plan -> Int
solve2 (Plan seeds arrows) =
  let mapper = foldl (\acc -> \(MapArrow f t m) -> m . acc) id arrows
      seed_ranges = seeds_as_ranges seeds
      checker (i1, v1) (i2, v2) =
        if (i2 - i1) == (v2 - v1)
          then Skip
          else if abs (i2 - i1) == 1
                 then Found (i2, v2)
                 else Continue
      results =
        join $
        (\(from, len) -> seeker (from) (from + len + 1) mapper checker) <$>
        seed_ranges
   in minimum $ snd <$> results

data SeekResult
  = Skip
  | Continue
  | Found (Int, Int)

-- A magical binary search that will search the entire space looking for
-- interesting features (such as jumps in continuous space)
seeker ::
     Int
  -> Int
  -> (Int -> Int)
  -> ((Int, Int) -> (Int, Int) -> SeekResult)
  -> [(Int, Int)]
seeker start stop produce continuous =
  let startv = produce start
      stopv = produce stop
   in case continuous (start, startv) (stop, stopv) of
        Skip -> []
        Found v -> [v]
        Continue ->
          let mid = (start + stop) `div` 2
           in seeker start mid produce continuous ++
              seeker (mid + 1) stop produce continuous

solve :: IO (Solution Int)
solve = do
  input <- run_parser parse_plan <$> get_puzzle_input Mine 2023 5
  sol_1 <- pure $ solve1 input
  sol_2 <- pure $ solve2 input
  pure $ SolvedTwo 2023 5 sol_1 (Revealed 486613012) sol_2 (Revealed 56931769)
