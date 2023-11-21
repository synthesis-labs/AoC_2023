{-# LANGUAGE QuasiQuotes #-}

module AoC where

import           Data.String.Interpolate (i)
import           Text.Printf             (printf)

data Solution a
  = NoSolution Int
  | SolvedOne Int a (Maybe a)
  | SolvedTwo Int a (Maybe a) a (Maybe a)

print_sol :: (Show a, Eq a) => a -> Maybe a -> String
print_sol candidate Nothing = [i|#{show candidate} ❓|]
print_sol candidate (Just answer) =
  [i|#{show candidate} #{if candidate == answer
            then "✅"
            else "❌ (should be " <> show answer <> ")"
        }|]

instance (Show a, Eq a) => Show (Solution a) where
  show (NoSolution day) =
    [i|[SOLUTION] Day #{printf "%02d" day :: String}: None yet ... 🧐
    |] :: String
  show (SolvedOne day q1 q1') =
    [i|[SOLUTION] Day #{printf "%02d" day :: String}: A => #{print_sol q1 q1'}
    |]
  show (SolvedTwo day q1 q1' q2 q2') =
    [i|[SOLUTION] Day #{printf "%02d" day :: String}: A => #{print_sol q1 q1'}
                   B => #{print_sol q2 q2'}
    |]
