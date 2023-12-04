module Day04 where

import           AoC
import           Control.Monad         (join)
import           Control.Monad.State   (State, get, put, runState)
import qualified Data.Map              as Map
import           Data.Maybe            (fromMaybe)
import qualified Data.Set              as Set
import           Debug.Trace           (trace)
import           Handy                 (WhichPuzzleInput (..), get_puzzle_input,
                                        unique)
import           Parsing               (run_parser, run_parser_with_state)
import           Text.Parsec           (Parsec, anyChar, char, choice, digit,
                                        getState, letter, many1, newline,
                                        optional, sepBy, setState, string, try,
                                        (<|>))

import           Data.Function.Memoize (Memoizable, memoize, memoize2, memoize3)

type Card = (Int, [Int], [Int])

parse_vals :: Parsec String () [Int]
parse_vals = do
  many1 $ do
    val :: Int <- read <$> (try $ many1 digit)
    _ <- optional $ many1 $ char ' '
    pure val

parse_card :: Parsec String () Card
parse_card = do
  _ <- string "Card" <* (many1 $ char ' ')
  num <- read <$> many1 digit
  _ <- (string ":") <* (many1 $ char ' ')
  i_have <- parse_vals
  _ <- string "|" <* (many1 $ char ' ')
  winning <- parse_vals
  newline
  pure $ (num, i_have, winning)

points :: [Int] -> Int
points [] = 0
points x  = 2 ^ (length x - 1)

solve1 :: [Card] -> Int
solve1 input =
  sum $
  fmap
    (\((n, m, w) :: (Int, [Int], [Int])) ->
       let mine = Set.fromList $ m
           wins = Set.fromList $ w
           int = points $ Set.toList $ Set.intersection mine wins
        in int)
    input

win_amount :: [Int] -> [Int] -> Int
win_amount m w =
  let mine = Set.fromList $ m
      wins = Set.fromList $ w
   in length $ Set.toList $ Set.intersection mine wins

fetch_cards :: [Card] -> Int -> Int -> [Card]
fetch_cards original cardnum n =
  let find_card original num = filter (\(n, _, _) -> n == num) original
   in join $ [find_card original x | x <- [cardnum + 1 .. cardnum + n]]

type CardMap = Map.Map Int [Int]

win_map :: [Card] -> CardMap
win_map orig =
  Map.fromList $
  fmap
    (\(n, m, w) ->
       let !wins = win_amount m w
           !won_cards = fetch_cards orig n wins
        in (n, (\(n, _, _) -> n) <$> won_cards))
    orig

solve2 :: [Card] -> Int
solve2 input =
  let cards :: [Int] = (\(n, _, _) -> n) <$> input
      wmap :: CardMap = win_map input
      lookup_wins = memoize (\card -> fromMaybe [] $ Map.lookup card wmap)
      inner_reduce :: [Int] -> Int -> Int
      inner_reduce [] count = count
      inner_reduce (m:ms) count =
        inner_reduce ((lookup_wins m) ++ ms) (count + 1)
   in inner_reduce cards 0

solve :: IO (Solution Int)
solve = do
  input <- run_parser (many1 parse_card) <$> get_puzzle_input Mine 2023 4
  let solution_1 = solve1 input
  let solution_2 = solve2 input
  pure $
    SolvedTwo 2023 4 solution_1 (Revealed 21138) solution_2 (Revealed 7185540)
