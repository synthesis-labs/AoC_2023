module Day07 where

import           AoC
import           Control.Concurrent  (threadDelay)
import           Control.Monad       (join)
import           Control.Monad.State (State, get, put, runState)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import qualified Data.Set            as Set
import           Data.Sort
import           Debug.Trace         (trace)
import           Handy               (WhichPuzzleInput (..), get_puzzle_input,
                                      unique)
import           Parsing             (run_parser, run_parser_with_state)
import           Text.Parsec         (Parsec, anyChar, char, choice, digit,
                                      getState, letter, many1, newline,
                                      optional, sepBy, setState, string, try,
                                      (<|>))

data Card
  = Joker -- Added for part2
  | Face Int -- Assuming only 2-9 are valid for now
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord)

data Hand =
  Hand HandType Card Card Card Card Card
  deriving (Eq)

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Ord)

instance Ord Hand where
  compare (Hand t1 c1 c2 c3 c4 c5) (Hand t2 c6 c7 c8 c9 c10) =
    case compare t1 t2 of
      EQ -> compare ([c1, c2, c3, c4, c5]) ([c6, c7, c8, c9, c10])
      LT -> LT
      GT -> GT

parse_card :: Parsec String () Card
parse_card = do
  c <-
    choice
      [ Face <$> (read <$> (\c -> [c]) <$> digit)
      , Ten <$ char 'T'
      , Jack <$ char 'J'
      , Queen <$ char 'Q'
      , King <$ char 'K'
      , Ace <$ char 'A'
      ]
  pure c

parse_entry :: Parsec String () (Hand, Int)
parse_entry = do
  card1 <- parse_card
  card2 <- parse_card
  card3 <- parse_card
  card4 <- parse_card
  card5 <- parse_card
  bid <- read <$> ((many1 $ char ' ') *> many1 digit) <* newline
  let hand_type = handtype [card1, card2, card3, card4, card5]
  pure (Hand hand_type card1 card2 card3 card4 card5, bid)

handtype :: [Card] -> HandType
handtype cards =
  let grouped = groupSort (\c g -> (c, length g + 1)) cards
      ranked =
        foldr
          (\(card, count) rank ->
             let grouprank =
                   case (card, count) of
                     (Joker, 5) -> FiveOfAKind
                     (Joker, _) -> HighCard -- ignore jokers here
                     (_, 1)     -> HighCard
                     (_, 2)     -> OnePair
                     (_, 3)     -> ThreeOfAKind
                     (_, 4)     -> FourOfAKind
                     (_, 5)     -> FiveOfAKind
              in if sort [rank, grouprank] == [OnePair, OnePair]
                   then TwoPair
                   else if sort [rank, grouprank] == [OnePair, ThreeOfAKind]
                          then FullHouse
                          else if grouprank > rank
                                 then grouprank
                                 else rank)
          HighCard
          grouped
   in ranked

solve1 :: [(Hand, Int)] -> IO Int
solve1 hands = do
  let sorted = sortOn (\(h, _) -> h) hands
      ranked = zip [1 ..] sorted
      winnings = (\(rank, (_, bid)) -> bid * rank) <$> ranked
  pure $ sum $ winnings

reinterpret_for_part2 :: [(Hand, Int)] -> [(Hand, Int)]
reinterpret_for_part2 hands =
  let switch :: Card -> Card
      switch Jack = Joker
      switch c    = c
   in (\(Hand t c1 c2 c3 c4 c5, bid) ->
         let new_handtype =
               handtype
                 [ (switch c1)
                 , (switch c2)
                 , (switch c3)
                 , (switch c4)
                 , (switch c5)
                 ]
          in ( Hand
                 new_handtype
                 (switch c1)
                 (switch c2)
                 (switch c3)
                 (switch c4)
                 (switch c5)
             , bid)) <$>
      hands

apply_joker :: Int -> HandType -> HandType
apply_joker 0 hand = hand
apply_joker n hand =
  apply_joker (n - 1) $
  case hand of
    HighCard     -> OnePair
    OnePair      -> ThreeOfAKind
    TwoPair      -> FullHouse
    ThreeOfAKind -> FourOfAKind
    FullHouse    -> FourOfAKind
    FourOfAKind  -> FiveOfAKind
    FiveOfAKind  -> FiveOfAKind

count_jokers :: Hand -> Int
count_jokers (Hand t c1 c2 c3 c4 c5) =
  length $ filter (== Joker) [c1, c2, c3, c4, c5]

solve2 :: [(Hand, Int)] -> IO Int
solve2 input = do
  let hands = reinterpret_for_part2 input
      -- Convert the jacks to jokers, remembering to recalculate their
      -- hand types too (whoops)
      converted =
        (\(hand@(Hand t c1 c2 c3 c4 c5), bid) ->
           (Hand (apply_joker (count_jokers hand) t) c1 c2 c3 c4 c5, bid)) <$>
        hands
      -- Sort and rank
      ranked = zip [1 ..] $ sortOn (\(h, _) -> h) converted
      winnings = (\(rank, (_, bid)) -> bid * rank) <$> ranked
  pure $ sum $ winnings

solve :: IO (Solution Int)
solve = do
  input <- run_parser (many1 parse_entry) <$> get_puzzle_input Mine 2023 7
  solution_1 <- solve1 input
  solution_2 <- solve2 input
  pure $
    SolvedTwo
      2023
      7
      solution_1
      (Revealed 247815719)
      solution_2
      (Revealed 248747492)
