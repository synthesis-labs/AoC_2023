module Day10 where

import           Algorithm.Search    (dfs)
import           AoC
import           Control.Monad       (join)
import           Control.Monad.State (State, get, put, runState)
import           Data.List           ((\\))
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import qualified Data.Set            as Set
import           Debug.Trace         (trace)
import           Handy               (WhichPuzzleInput (..), get_puzzle_input,
                                      unique)
import           Parsing             (run_parser, run_parser_with_state)
import           Text.Parsec         (Parsec, anyChar, char, choice, digit,
                                      getPosition, getState, letter, many1,
                                      newline, optional, sepBy, setState,
                                      sourceColumn, sourceLine, string, try,
                                      (<|>))

data Pipe
  = Start_Finish -- S
  | None_None -- .
  | Up_Down -- |
  | UpLeft_RightDown -- 7
  | DownLeft_RightUp -- J
  | UpRight_LeftDown -- F
  | DownRight_LeftUp -- L
  | Left_Right -- -
  deriving (Eq, Ord)

instance Show Pipe where
  show Start_Finish     = "S"
  show None_None        = " " -- "░"
  show Up_Down          = "│"
  show UpLeft_RightDown = "┐"
  show DownLeft_RightUp = "┘"
  show UpRight_LeftDown = "┌"
  show DownRight_LeftUp = "└"
  show Left_Right       = "─"

type Position = (Int, Int)

type PipeMap = Map.Map Position Pipe

print_map :: PipeMap -> Map.Map Position Char -> String
print_map world note =
  let max_x = maximum (fst <$> Map.keys world) + 1
      min_x = minimum (fst <$> Map.keys world) - 1
      max_y = maximum (snd <$> Map.keys world) + 1
      min_y = minimum (snd <$> Map.keys world) - 1
      print_note (x, y) =
        fromMaybe (print_pipe (x, y)) (pure <$> Map.lookup (x, y) note)
      print_pipe (x, y) = fromMaybe " " (show <$> Map.lookup (x, y) world)
      rows =
        [ ([print_note (x, y) | x <- [min_x .. max_x]] <> ["\n"])
        | y <- [min_y .. max_y]
        ]
   in join $ join rows

parse_pipes :: Parsec String () (Position, Pipe)
parse_pipes = do
  pos <- getPosition
  let (x, y) = (sourceColumn pos, sourceLine pos) -- (1,1) is top left
  connect <-
    choice
      [ char 'S' *> pure Start_Finish
      , char '.' *> pure None_None
      , char '|' *> pure Up_Down
      , char '7' *> pure UpLeft_RightDown
      , char 'J' *> pure DownLeft_RightUp
      , char 'F' *> pure UpRight_LeftDown
      , char 'L' *> pure DownRight_LeftUp
      , char '-' *> pure Left_Right
      ]
  optional newline
  pure ((x, y), connect)

data Direction
  = Top'
  | Bottom'
  | Left'
  | Right'
  deriving (Show, Eq)

connections :: Pipe -> [Direction]
connections Start_Finish     = [Top', Bottom', Left', Right']
connections None_None        = []
connections Up_Down          = [Top', Bottom']
connections UpLeft_RightDown = [Left', Bottom']
connections DownLeft_RightUp = [Left', Top']
connections UpRight_LeftDown = [Right', Bottom']
connections DownRight_LeftUp = [Right', Top']
connections Left_Right       = [Left', Right']

opposite :: Direction -> Direction
opposite Top'    = Bottom'
opposite Bottom' = Top'
opposite Left'   = Right'
opposite Right'  = Left'

valid_connection :: Pipe -> Direction -> Pipe -> Bool
valid_connection from dir to =
  let from_pipe_connects = filter ((==) dir) $ connections from
      to_pipe_connects = filter ((==) (opposite dir)) $ connections to
   in length from_pipe_connects > 0 && length to_pipe_connects > 0

lookup_pipe_by_position :: PipeMap -> Position -> Maybe Pipe
lookup_pipe_by_position world pos = Map.lookup pos world

pipes_around :: PipeMap -> Position -> [(Position, Pipe)]
pipes_around world current_pos@(x, y) =
  let current_pipe =
        case lookup_pipe_by_position world current_pos of
          Nothing -> error "Somehow you have fallen off the world!"
          Just p  -> p
      around =
        [ (Top', (x, y - 1))
        , (Bottom', (x, y + 1))
        , (Left', (x - 1, y))
        , (Right', (x + 1, y))
        ]
      possibles =
        concatMap
          (\(dir, pos) ->
             case lookup_pipe_by_position world pos of
               Nothing -> []
               Just other ->
                 if valid_connection current_pipe dir other
                   then [(pos, other)]
                   else [])
          around
   in possibles

circuit :: PipeMap -> [Position]
circuit world =
  let start_pos =
        fst $ head $ filter (\(pos, p) -> p == Start_Finish) $ Map.toList world
      loops =
        dfs
          (\(prev :: (Position, Pipe), curr :: (Position, Pipe)) ->
             let (current_pos, current_pipe) = curr
                 options = pipes_around world current_pos \\ [prev] -- no backtracking
              in ((curr, )) <$> options -- haha good luck figuring this out!
           )
          (\(prev :: (Position, Pipe), curr :: (Position, Pipe)) ->
             let (current_pos, current_pipe) = curr
                 options = pipes_around world current_pos
              -- Have we moved away from the start and now see the start?
              in (snd prev) /= Start_Finish &&
                 Start_Finish `elem` (snd <$> options))
          ((start_pos, Start_Finish), (start_pos, Start_Finish))
   in case loops of
        Nothing    -> error "Unable to solve"
        Just paths -> start_pos : (fst <$> snd <$> (paths))

solve1 :: [(Position, Pipe)] -> IO Int
solve1 input =
  let world = Map.fromList input
   in pure $ ((length $ circuit world) + 1) `div` 2

solve2 :: [(Position, Pipe)] -> IO Int
solve2 input =
  let world = Map.fromList input
      max_x = maximum (fst <$> Map.keys world) + 1
      min_x = minimum (fst <$> Map.keys world) - 1
      enclosure = circuit world
      enclosure_map = Map.fromList $ (, True) <$> enclosure
      -- Patch the starting position
      start_pos =
        fst $ head $ filter (\(pos, p) -> p == Start_Finish) $ Map.toList world
      -- patched_world = Map.insert start_pos UpRight_LeftDown $ Map.fromList input
      patched_world = Map.insert start_pos Up_Down $ Map.fromList input
      inside :: Position -> Int
      inside (x, y) =
        if x > max_x
          then 0
          else if (Map.lookup (x, y) enclosure_map) == Just True
                 then case Map.lookup (x, y) patched_world of
                        Just Up_Down          -> inside (x + 1, y) + 2
                        Just UpLeft_RightDown -> inside (x + 1, y) - 1
                        Just DownLeft_RightUp -> inside (x + 1, y) + 1
                        Just UpRight_LeftDown -> inside (x + 1, y) + 1
                        Just DownRight_LeftUp -> inside (x + 1, y) - 1
                        _                     -> inside (x + 1, y)
                 else inside (x + 1, y)
   in do let dummy = 1
        --  putStrLn $ print_map patched_world Map.empty
        --  putStrLn $
        --    print_map
        --      (Map.fromList $
        --       filter (\(pos, p) -> (Map.lookup pos enclosure_map) == Just True) $
        --       Map.toList patched_world)
        --      (Map.fromList $
        --       (\pos ->
        --          ( pos
        --          , if even $ (inside pos `div` 2)
        --              then '.'
        --              else '*')) <$>
        --       (fst <$> Map.toList patched_world) \\ enclosure)
         pure $
           length $
           filter odd $
           (\v -> v `div` 2) <$> inside <$>
           ((fst <$> Map.toList patched_world) \\ enclosure)

solve :: IO (Solution Int)
solve = do
  input <- run_parser (many1 parse_pipes) <$> get_puzzle_input Mine 2023 10
  solution_1 <- solve1 input
  solution_2 <- solve2 input
  pure $ SolvedTwo 2023 10 solution_1 (Revealed 7086) solution_2 (Revealed 317)
