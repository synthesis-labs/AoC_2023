module Day04 where

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

type Card = (Int, [Int], [Int])

parse_vals :: Parsec String () [Int]
parse_vals = do
  many1 $ read <$> (try $ many1 digit) <* (optional $ many1 $ char ' ')

parse_card :: Parsec String () Card
parse_card = do
  card_num <- (string "Card" <* (many1 $ char ' ')) *> (read <$> many1 digit)
  i_have <- ((string ":") <* (many1 $ char ' ')) *> parse_vals
  winning <- (string "|" <* (many1 $ char ' ')) *> parse_vals <* newline
  pure $ (card_num, i_have, winning)

solve1 :: [Card] -> Int
solve1 input =
  let points :: [Int] -> Int
      points [] = 0
      points x  = 2 ^ (length x - 1)
   in sum $
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
       let wins = win_amount m w
           won_cards = fetch_cards orig n wins
        in (n, (\(n, _, _) -> n) <$> won_cards))
    orig

solve2 :: [Card] -> Int
solve2 input =
  let cards :: [Int] = (\(n, _, _) -> n) <$> input
      wmap :: CardMap = win_map input
      lookup_wins card = fromMaybe [] $ Map.lookup card wmap
      -- Looks like a manual fold to me :)
      reduce :: [Int] -> Int -> Int
      reduce [] count     = count
      reduce (m:ms) count = reduce ((lookup_wins m) ++ ms) (count + 1)
   in reduce cards 0

solve :: IO (Solution Int)
solve = do
  input <- run_parser (many1 parse_card) <$> get_puzzle_input Mine 2023 4
  let solution_1 = solve1 input
  let solution_2 = solve2 input
  pure $
    SolvedTwo 2023 4 solution_1 (Revealed 21138) solution_2 (Revealed 7185540)
