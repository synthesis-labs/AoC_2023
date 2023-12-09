module Day08 where

import           AoC
import           Control.Monad       (join)
import           Control.Monad.State (State, get, put, runState)
import           Data.List           (isSuffixOf)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import qualified Data.Set            as Set
import           Data.Strings        (strEndsWith)
import           Debug.Trace         (trace)
import           Handy               (WhichPuzzleInput (..), get_puzzle_input,
                                      unique)
import           Parsing             (run_parser, run_parser_with_state)
import           Text.Parsec         (Parsec, alphaNum, anyChar, char, choice,
                                      digit, getState, letter, many1, newline,
                                      optional, sepBy, setState, string, try,
                                      (<|>))

data Instruction
  = LEFT
  | RIGHT
  deriving (Show)

walk ::
     [(String, String, String)]
  -> [Instruction]
  -> (String, String, String)
  -> [Instruction]
  -> Int
walk tmap orig_ins x [] = walk tmap orig_ins x orig_ins
walk tmap orig_ins ("ZZZ", _, _) _ = 0
walk tmap orig_ins (key, left', right') (LEFT:inss) =
  let left = head $ filter (\(k, _, _) -> k == left') tmap
   in 1 + walk tmap orig_ins left inss
walk tmap orig_ins (key, left', right') (RIGHT:inss) =
  let right = head $ filter (\(k, _, _) -> k == right') tmap
   in 1 + walk tmap orig_ins right inss

parse_instruction :: Parsec String () Instruction
parse_instruction = do
  v <- letter
  if v == 'L'
    then pure LEFT
    else pure RIGHT

parse_map_entry :: Parsec String () (String, String, String)
parse_map_entry = do
  key <- many1 alphaNum <* string " = ("
  left <- many1 alphaNum <* string ", "
  right <- many1 alphaNum <* char ')'
  pure (key, left, right)

parse_map :: Parsec String () ([Instruction], [(String, String, String)])
parse_map = do
  instructions <- many1 parse_instruction
  newline
  newline
  maps <- many1 (parse_map_entry <* newline)
  pure $ (instructions, maps)

solve1 :: ([Instruction], [(String, String, String)]) -> IO Int
solve1 (ins, tmap) =
  let start = head $ filter (\(k, _, _) -> k == "AAA") tmap
   in pure $ walk tmap ins start ins

-- This obviously didn't work, but kept for fun :)
brutewalk ::
     [(String, String, String)]
  -> [Instruction]
  -> [(String, String, String)]
  -> [Instruction]
  -> Int
-- Reboot the instruction loop
brutewalk tmap orig_ins nodes_we_at [] =
  brutewalk tmap orig_ins nodes_we_at orig_ins
brutewalk tmap orig_ins nodes_we_at (i:inss) =
  if all (\(k, _, _) -> "Z" `isSuffixOf` k) nodes_we_at
    then 0
    else case i of
           LEFT ->
             let next = ((\(_, l, r) -> l) <$> nodes_we_at)
                 nextp = filter (\(k, _, _) -> k `elem` next) tmap
              in 1 + brutewalk tmap orig_ins nextp inss
           RIGHT ->
             let next = ((\(_, l, r) -> r) <$> nodes_we_at)
                 nextp = filter (\(k, _, _) -> k `elem` next) tmap
              in 1 + brutewalk tmap orig_ins nextp inss

single ::
     [(String, String, String)]
  -> [Instruction]
  -> (String, String, String)
  -> [Instruction]
  -> Int
  -> Int
single tmap orig_ins x [] acc = single tmap orig_ins x orig_ins acc
single tmap orig_ins (key, left', right') (LEFT:inss) acc =
  if "Z" `isSuffixOf` key
    then acc
    else let left = head $ filter (\(k, _, _) -> k == left') tmap
          in single tmap orig_ins left inss (acc + 1)
single tmap orig_ins (key, left', right') (RIGHT:inss) acc =
  if "Z" `isSuffixOf` key
    then acc
    else let right = head $ filter (\(k, _, _) -> k == right') tmap
          in single tmap orig_ins right inss (acc + 1)

solve2 :: ([Instruction], [(String, String, String)]) -> IO Int
solve2 (ins, tmap) = do
  let starts = filter (\(k, _, _) -> "A" `isSuffixOf` k) tmap
  let lengths_of_each = (\start -> single tmap ins start ins 0) <$> starts
  let answer = foldl lcm 1 lengths_of_each
  pure $ answer

solve :: IO (Solution Int)
solve = do
  input <- run_parser parse_map <$> get_puzzle_input Mine 2023 8
  solution_1 <- solve1 input
  solution_2 <- solve2 input
  pure $
    SolvedTwo
      2023
      8
      solution_1
      (Revealed 14257)
      solution_2
      (Revealed 16187743689077)
