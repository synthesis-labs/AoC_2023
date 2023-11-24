# Advent of code Haskell blueprint

Install Haskell toolchain via [GHCup](https://www.haskell.org/ghcup/install/)

```
cabal run
```

## Blueprint

```hs
module Day01 where

solveOne :: String -> String
solveOne = show . length

run :: IO ()
run = do
  contents <- lines <$> readFile "./data/day01-test"
  let answers = [solveOne x | x <- contents]
  mapM_ print answers
  let answer = foldl (+) 0 answers
  putStrLn $ "Answer Q1 => " ++ show answer
```

## Explanation

The folder structure:

```
- app
  - Day01.hs
  ...
  - Day25.hs
  - Main.hs
  - Utils.hs
- data
  - day01
  - day01-test
  ...
  - day25
  - day25-test
- aoc.cabal
```

We create a module for each day's puzzle:

```hs
module Day01 where
```

Add the module to our Main function:

```hs
module Main where

import qualified Day01 (run) -- <---------
-- ...
import qualified Day25 (run)

main :: IO ()
main = do
  Day01.run -- <---------
```

You'll also need to add it to the cabal file:

```
executable aoc
    main-is:          Main.hs
    other-modules:    Utils
                      Day01 <---------
                      ...
                      Day25
```

Let's first look at the Day01.hs run function:

```hs
run :: IO ()
run = do
  -- Read the contents of the puzzle into [String]
  contents <- lines <$> readFile "./data/day04-test"

  -- Get a list of answers for each line
  let answers = [solveOne x | x <- contents]

  -- Print each line's solution
  mapM_ print answers

  -- Fold (Reduce) to get the full answer (Here you might append strings ++, or sum up +)
  let answer = foldl (+) 0 answers
  -- Again, point-free style, equivalent to: let answer = foldl (\x acc -> x ++ acc) "" answers

  putStrLn $ "Answer Q1 => " ++ show answer
```

The solveOne function solves one line of input, here we're just getting the length as an example:

```hs
solveOne :: String -> String
solveOne = show . length
-- We're using point-free style, equivalent to: solveOne input = show (length input)
```

## Learning functional programming

- For Haskell, [this](http://learnyouahaskell.com/chapters) is great
- Here's [my repo](https://github.com/tjaard123/play-functional) dedicated to this, with some resources