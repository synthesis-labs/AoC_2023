module Day03 where

import           AoC
import           Control.Monad (join)
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Handy         (WhichPuzzleInput (..), get_puzzle_input, unique)
import           Parsing       (run_parser, run_parser_with_state)
import           Text.Parsec   (Parsec, anyChar, char, choice, digit, getState,
                                letter, many1, newline, optional, setState,
                                string, (<|>))

data Element
  = Symbol Char
  | PartNumber Int
  | Blank
  deriving (Eq, Show)

type Position = (Int, Int)

data Scheme =
  Scheme [(Element, [Position])]
  deriving (Show)

inc_pos :: Int -> Int -> Parsec String Position [Position]
inc_pos x y = do
  (cur_x, cur_y) <- getState
  _ <- setState $ (cur_x + x, cur_y + y)
  pure $ [(x', y') | x' <- [cur_x .. ((cur_x + x) - 1)], y' <- [cur_y]]

parse_part_number :: Parsec String Position (Element, [Position])
parse_part_number = do
  val :: Int <- read <$> many1 digit
  pos's <- inc_pos (length $ show val) 0
  pure $ (PartNumber val, pos's)

parse_symbol :: Parsec String Position (Element, [Position])
parse_symbol = do
  symbol <- anyChar
  pos's <- inc_pos 1 0
  pure $ (Symbol symbol, pos's)

parse_blank :: Parsec String Position (Element, [Position])
parse_blank = do
  _ <- char '.'
  pos's <- inc_pos 1 0
  pure $ (Blank, pos's)

parse_newline :: Parsec String Position ()
parse_newline = do
  _ <- newline
  (cur_x, cur_y) <- getState
  _ <- setState (0, cur_y + 1)
  pure ()

parse_scheme :: Parsec String Position Scheme
parse_scheme = do
  elements <-
    many1 $
    (choice [parse_part_number, parse_blank, parse_symbol]) <*
    (optional parse_newline)
  pure $ Scheme elements

adjacent :: Position -> Position -> Bool
adjacent (x, y) (a, b) = (abs (x - a) <= 1) && (abs (y - b) <= 1)

symbols :: Scheme -> [Position]
symbols (Scheme scheme) =
  concatMap
    (\(ele, pos's) ->
       case ele of
         Symbol _ -> pos's
         _        -> [])
    scheme

specific_symbols :: Char -> Scheme -> [Position]
specific_symbols matching (Scheme scheme) =
  concatMap
    (\(ele, pos's) ->
       case ele of
         Symbol matching -> pos's
         _               -> [])
    scheme

parts :: Scheme -> [(Int, [Position])]
parts (Scheme scheme) =
  concatMap
    (\(ele, pos's) ->
       case ele of
         PartNumber num -> [(num, pos's)]
         _              -> [])
    scheme

solve1 :: Scheme -> Int
solve1 scheme@(Scheme flat_scheme)
  -- Find all the part numbers that are adjacent to symbols
 =
  let part_numbers_adjacent_to_symbols :: [(Int, [Position])] =
        filter
          (\(num, pos's) ->
             any (\pos -> any (adjacent pos) (symbols scheme)) pos's)
          (parts scheme)
   in sum $ fst <$> part_numbers_adjacent_to_symbols

solve2 :: Scheme -> Int
solve2 scheme@(Scheme flat_scheme)
  -- Get all the gears (have exactly two part numbers adjacent to them) and add
  -- up their "ratios" (part1 * part2)
 =
  let gears :: [(Position, Int)]
      gears =
        concatMap
          (\(possible_gear :: Position) ->
             let adjacent_parts :: [Int] =
                   unique $
                   concatMap
                     (\(num :: Int, pos :: [Position]) ->
                        if any (\pos' -> adjacent pos' possible_gear) pos
                          then [num]
                          else [])
                     (parts scheme)
              -- If exactly two parts, then calculate their "ratio"
              -- otherwise it's not a gear
              in case adjacent_parts of
                   (part_1:part_2:[]) -> [(possible_gear, part_1 * part_2)]
                   _                  -> [])
          (specific_symbols '*' scheme)
   in sum $ snd <$> gears

solve :: IO (Solution Int)
solve = do
  input <-
    run_parser_with_state (parse_scheme) (0, 0) <$> get_puzzle_input Mine 2023 3
  let solution_1 = solve1 input
      solution_2 = solve2 input
  pure $
    SolvedTwo 2023 3 solution_1 (Revealed 526404) solution_2 (Revealed 84399773)
