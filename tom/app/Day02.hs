module Day02 where

import           AoC
import           Control.Monad (join)
import           Handy         (WhichPuzzleInput (..), get_puzzle_input)
import           Parsing       (run_parser)
import           Text.Parsec   (Parsec, choice, digit, many1, newline, optional,
                                string, (<|>))

type Round = [(Colour, Int)]

data Game =
  Game Int [Round]

data Colour
  = Red
  | Green
  | Blue
  deriving (Eq)

parse_colour :: Parsec String () Colour
parse_colour = do
  colour <-
    choice
      [ string "red" *> pure Red
      , string "green" *> pure Green
      , string "blue" *> pure Blue
      ]
  pure colour

parse_round :: Parsec String () Round
parse_round = do
  round' :: [(Colour, Int)] <-
    many1 $ do
      count <- read <$> many1 digit <* string " "
      colour <- parse_colour <* (optional $ string ", ")
      pure (colour, count)
  _ <- optional ((newline *> pure "") <|> string "; ")
  pure round'

parse_game :: Parsec String () Game
parse_game = do
  number <- string "Game " *> (read <$> many1 digit)
  rounds <- string ": " *> many1 parse_round
  pure $ Game number rounds

valid :: Round -> Bool
valid =
  foldr (&&) True .
  fmap
    (\case
       (Red, c)   -> c <= 12
       (Green, c) -> c <= 13
       (Blue, c)  -> c <= 14)

solve1 :: [Game] -> Int
solve1 (Game number rounds:gs) =
  if all (== True) $ valid <$> rounds
    then number + solve1 gs
    else solve1 gs
solve1 [] = 0

min_cubes :: [Round] -> (Int, Int, Int)
min_cubes rounds =
  let max_red = maximum $ snd <$> filter ((== Red) . fst) (join rounds)
      max_green = maximum $ snd <$> filter ((== Green) . fst) (join rounds)
      max_blue = maximum $ snd <$> filter ((== Blue) . fst) (join rounds)
   in (max_red, max_green, max_blue)

power :: (Int, Int, Int) -> Int
power (r, g, b) = r * g * b

solve2 :: [Game] -> Int
solve2 games = sum $ power . min_cubes <$> all_rounds
  where
    all_rounds = (\(Game _ r) -> r) <$> games

solve :: IO (Solution Int)
solve = do
  input <- run_parser (many1 parse_game) <$> get_puzzle_input Mine 2023 2
  let solution_1 = solve1 input
  let solution_2 = solve2 input
  pure $ SolvedTwo 2023 2 solution_1 (Revealed 2449) solution_2 (Revealed 63981)
