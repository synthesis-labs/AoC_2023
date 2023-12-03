module Day03 where

import           AoC
import           Control.Monad (join)
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Handy         (WhichPuzzleInput (..), get_puzzle_input)
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

type SchemeMap = Map.Map Position Element

build_scheme_map :: Scheme -> SchemeMap
build_scheme_map (Scheme scheme) = Map.fromList $ join $ fmap f scheme
  where
    f (element, positions) = fmap (\pos -> (pos, element)) positions

adjacent :: Position -> Position -> Bool
adjacent (x, y) (a, b) = (abs (x - a) <= 1) && (abs (y - b) <= 1)

solve1 scheme@(Scheme flat_scheme)
  -- All the part numbers that are adjacent to a symbol, add them up
 =
  let map = build_scheme_map scheme
      -- Can't rely on the scheme map for dealing with part numbers, because
      -- they have multiple positions, some of which are adjacent to symbols
      part_numbers :: [(Int, [Position])] =
        concatMap
          (\(pos, pos's) ->
             case pos of
               PartNumber num -> [(num, pos's)]
               _              -> []) $
        flat_scheme
      -- Symbols take up only a single space, so this is safe (?)
      symbols =
        concatMap
          (\(pos, ele) ->
             case ele of
               Symbol _ -> [pos]
               _        -> []) $
        Map.toList map
      -- Find all the part numbers that are adjacent to symbols
      part_numbers_adjacent_to_symbols :: [(Int, [Position])] =
        filter
          (\(num, pos's) ->
             any
               (\pos -> any (\symbol_pos -> adjacent pos symbol_pos) symbols)
               pos's)
          part_numbers
   in sum $ (\(num, _) -> num) <$> part_numbers_adjacent_to_symbols

solve2 :: Scheme -> Int
solve2 scheme@(Scheme flat_scheme)
  -- Get all the gears (have exactly two part numbers adjacent to them) and add
  -- them up
 =
  let map = build_scheme_map scheme
      part_numbers :: [(Int, [Position])] =
        concatMap
          (\(pos, pos's) ->
             case pos of
               PartNumber num -> [(num, pos's)]
               _              -> []) $
        flat_scheme
      -- Get all symbols which have '*' as their value
      gears :: [Position] =
        concatMap
          (\(pos, pos's) ->
             case pos of
               Symbol '*' -> pos's
               _          -> []) $
        flat_scheme
      -- Get all the part numbers that are adjacent to gears
      gears_with_adjacent_part_numbers :: [(Position, Int)]
      gears_with_adjacent_part_numbers =
        concatMap
          (\(gear :: Position) ->
             let adjacent_parts :: [Int] =
                   concatMap
                     (\(num :: Int, pos :: [Position]) ->
                        if any (\pos' -> adjacent pos' gear) pos
                          then [num]
                          else [])
                     part_numbers
                 uniq_parts = Set.toList $ Set.fromList $ adjacent_parts
              in if length uniq_parts == 2
                   then [(gear, ((uniq_parts !! 0) * (uniq_parts !! 1)))]
                   else [])
          gears
   in sum $ snd <$> gears_with_adjacent_part_numbers

solve :: IO (Solution Int)
solve = do
  input <-
    run_parser_with_state (parse_scheme) (0, 0) <$> get_puzzle_input Mine 2023 3
  let solution_1 = solve1 input
      solution_2 = solve2 input
  pure $
    SolvedTwo 2023 3 solution_1 (Revealed 526404) solution_2 (Revealed 84399773)
