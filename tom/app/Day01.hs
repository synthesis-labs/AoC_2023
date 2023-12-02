module Day01 where

import           AoC
import           Data.Char   (digitToInt)
import           Handy       (WhichPuzzleInput (..), get_puzzle_input)
import           Parsing     (run_parser)
import           Text.Parsec (Parsec, choice, digit, letter, many1, newline,
                              optional, string, try, (<|>))

data Sig
  = Sig Int
  | SigWord Int
  | Noise Char
  deriving (Show)

type WordMap = [(String, Int)]

word_map :: WordMap
word_map =
  [ ("one", 1)
  , ("two", 2)
  , ("three", 3)
  , ("four", 4)
  , ("five", 5)
  , ("six", 6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine", 9)
  ]

reverse_word_map :: WordMap -> WordMap
reverse_word_map wmap = (\(w, v) -> (reverse w, v)) <$> wmap

parse_word :: WordMap -> Parsec String () Int
parse_word wmap =
  let word_parsers :: [Parsec String () Int] =
        (\(w :: String, v :: Int) -> try (string w *> pure v)) <$> wmap
   in choice word_parsers

parse_signals :: WordMap -> Parsec String () [Sig]
parse_signals wmap =
  many1
    ((SigWord <$> parse_word wmap) <|> (Sig <$> digitToInt <$> digit) <|>
     (Noise <$> letter)) <*
  optional newline

clean :: [Sig] -> [Int]
clean signals =
  concatMap
    (\case
       Sig v     -> [v]
       SigWord v -> [v]
       _         -> [])
    signals

first_and_last_signal :: WordMap -> String -> (Int, Int)
first_and_last_signal wmap input =
  let forward = run_parser (parse_signals wmap) input
      backward =
        run_parser (parse_signals $ reverse_word_map wmap) $ reverse input
   in (head $ clean forward, head $ clean backward)

glue_together :: (Int, Int) -> Int
glue_together (f, l) = read $ (show f) <> (show l)

solve :: IO (Solution Int)
solve = do
  input <- get_puzzle_input Mine 2023 1
  let solution_1 =
        sum $ glue_together . first_and_last_signal [] <$> lines input
  let solution_2 =
        sum $ glue_together . first_and_last_signal word_map <$> lines input
  pure $
    SolvedTwo 2023 1 solution_1 (Revealed 53386) solution_2 (Revealed 53312)
