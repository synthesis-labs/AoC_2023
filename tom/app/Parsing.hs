module Parsing where

import           Text.Parsec (Parsec, char, digit, many1, newline, optional,
                              runParser)

-- An example parser
pair :: Parsec String () (Int, Int)
pair = do
  x <- read <$> many1 digit
  _ <- char ','
  y <- read <$> many1 digit
  pure (x, y)

-- An example parser
many_pairs :: Parsec String () [(Int, Int)]
many_pairs = many1 $ pair <* (optional newline)

-- Run parser or die!
run_parser :: Parsec String () a -> String -> a
run_parser parser input =
  case runParser parser () "(input)" input of
    Left err -> error $ "A terribly unfortunate parsing error: " ++ (show err)
    Right a  -> a

run_parser_with_state :: Parsec String s a -> s -> String -> a
run_parser_with_state parser start_state input =
  case runParser parser start_state "(input)" input of
    Left err -> error $ "A terribly unfortunate parsing error: " ++ (show err)
    Right a  -> a
