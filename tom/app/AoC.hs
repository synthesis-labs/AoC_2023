{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

module AoC where

import           Data.String.Interpolate (i)
import           Text.Printf             (printf)

type Year = Int

type Day = Int

data RealAnswer a
  = Unknown
  | Revealed a

data Solution a
  = NoSolution Year Day
  | SolvedOne Year Day a (RealAnswer a)
  | SolvedTwo Year Day a (RealAnswer a) a (RealAnswer a)

print_sol :: (Show a, Eq a) => a -> RealAnswer a -> String
print_sol candidate Unknown = [i|#{show candidate} ❓|]
print_sol candidate (Revealed answer) =
  [i|#{show candidate} #{if candidate == answer
            then "✅"
            else "❌ (should be " <> show answer <> ")"
        }|]

instance (Show a, Eq a) => Show (Solution a) where
  show (NoSolution year day) =
    [i|[#{pet year day} #{year} / #{printf "%02d" day :: String}] None yet ... 🧐
    |] :: String
  show (SolvedOne year day q1 q1') =
    [i|[#{pet year day} #{year} / #{printf "%02d" day :: String}] A => #{print_sol q1 q1'}
    |]
  show (SolvedTwo year day q1 q1' q2 q2') =
    [i|[#{pet year day} #{year} / #{printf "%02d" day :: String}] A => #{print_sol q1 q1'}
               B => #{print_sol q2 q2'}
    |]

pet :: Year -> Day -> String
pet year day = pets !! ((year + day - 1) `mod` (length pets))
  where
    pets =
      [ "🐱"
      , "🐶"
      , "🐭"
      , "🐹"
      , "🐰"
      , "🦊"
      , "🐻"
      , "🐼"
      , "🐨"
      , "🐯"
      , "🦁"
      , "🐮"
      , "🐷"
      , "🐸"
      , "🐵"
      , "🐔"
      , "🐧"
      , "🐦"
      , "🐤"
      , "🐴"
      , "🦄"
      , "🐝"
      , "🐛"
      , "🦋"
      , "🐌"
      , "🐞"
      , "🐜"
      , "🐢"
      , "🐍"
      , "🦎"
      , "🐕"
      , "🐩"
      , "🐈"
      , "🐓"
      , "🦃"
      , "🐇"
      , "🐁"
      , "🐀"
      , "🐿"
      , "🦔"
      ]
