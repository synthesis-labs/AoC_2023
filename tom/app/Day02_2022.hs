module Day02_2022 where

import           AoC
import           Handy       (get_puzzle_input)
import           Parsing     (run_parser)
import           Text.Parsec (Parsec, char, count, many1, newline, optional,
                              (<|>))

data First
  = A
  | B
  | C
  deriving (Show)

data Second
  = X
  | Y
  | Z
  deriving (Show)

-- Parse lines of:
-- A X
-- C Y
-- ..
pairs :: Parsec String () [(First, Second)]
pairs =
  many1 $ do
    first <- (rep 'A' A <|> rep 'B' B <|> rep 'C' C) <* char ' '
    second <- (rep 'X' X <|> rep 'Y' Y <|> rep 'Z' Z) <* newline
    pure (first, second)
  where
    rep from to = char from >>= (const $ pure to)

calc :: Int -> [(First, Second)] -> Int
calc acc ((A, X):xs) = calc (acc + 1 + 3) xs
calc acc ((B, Y):xs) = calc (acc + 2 + 3) xs
calc acc ((C, Z):xs) = calc (acc + 3 + 3) xs
calc acc ((A, Y):xs) = calc (acc + 2 + 6) xs
calc acc ((A, Z):xs) = calc (acc + 3 + 0) xs
calc acc ((B, X):xs) = calc (acc + 1 + 0) xs
calc acc ((B, Z):xs) = calc (acc + 3 + 6) xs
calc acc ((C, X):xs) = calc (acc + 1 + 6) xs
calc acc ((C, Y):xs) = calc (acc + 2 + 0) xs
calc acc []          = acc -- Guaranteed that this is the only remaining scenario

solve :: IO (Solution Int)
solve = do
  input <- run_parser pairs <$> get_puzzle_input 2022 2 -- Get the input
  let solution1 = calc 0 input
  pure $ SolvedOne 2022 25 solution1 (Revealed 13268)
