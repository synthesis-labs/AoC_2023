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

pairs :: Parsec String () [(First, Second)]
pairs =
  many1 $ do
    first <- (rep 'A' A <|> rep 'B' B <|> rep 'C' C) <* char ' '
    second <- (rep 'X' X <|> rep 'Y' Y <|> rep 'Z' Z) <* newline
    pure (first, second)
  where
    rep from to = char from >>= (const $ pure to)

calc :: Int -> [(First, Second)] -> Int
calc a ((A, X):xs) = calc (a + 1 + 3) xs
calc a ((B, Y):xs) = calc (a + 2 + 3) xs
calc a ((C, Z):xs) = calc (a + 3 + 3) xs
calc a ((A, Y):xs) = calc (a + 2 + 6) xs
calc a ((A, Z):xs) = calc (a + 3 + 0) xs
calc a ((B, X):xs) = calc (a + 1 + 0) xs
calc a ((B, Z):xs) = calc (a + 3 + 6) xs
calc a ((C, X):xs) = calc (a + 1 + 6) xs
calc a ((C, Y):xs) = calc (a + 2 + 0) xs
calc a []          = a -- Guaranteed that this is the only remaining scenario

solve :: IO (Solution Int)
solve = do
  input <- get_puzzle_input 2022 2
  let input' = run_parser pairs input
  pure $ SolvedOne 2022 25 (calc 0 input') (Just 13268)
