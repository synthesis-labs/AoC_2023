package io.nanovc.aoc2023.day07q2;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.*;

import static io.nanovc.aoc2023.day07q2.Day07Q2Tests.Solution1Tests.Card.*;
import static io.nanovc.aoc2023.day07q2.Day07Q2Tests.Solution1Tests.HandType.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * --- Part Two ---
 * To make things a little more interesting, the Elf introduces one additional rule. Now, J cards are jokers - wildcards that can act like whatever card would make the hand the strongest type possible.
 *
 * To balance this, J cards are now the weakest individual cards, weaker even than 2. The other cards stay in the same order: A, K, Q, T, 9, 8, 7, 6, 5, 4, 3, 2, J.
 *
 * J cards can pretend to be whatever card is best for the purpose of determining hand type; for example, QJJQ2 is now considered four of a kind. However, for the purpose of breaking ties between two hands of the same type, J is always treated as J, not the card it's pretending to be: JKKK2 is weaker than QQQQ2 because J is weaker than Q.
 *
 * Now, the above example goes very differently:
 *
 * 32T3K 765
 * T55J5 684
 * KK677 28
 * KTJJT 220
 * QQQJA 483
 * 32T3K is still the only one pair; it doesn't contain any jokers, so its strength doesn't increase.
 * KK677 is now the only two pair, making it the second-weakest hand.
 * T55J5, KTJJT, and QQQJA are now all four of a kind! T55J5 gets rank 3, QQQJA gets rank 4, and KTJJT gets rank 5.
 * With the new joker rule, the total winnings in this example are 5905.
 *
 * Using the new joker rule, find the rank of every hand in your set. What are the new total winnings?
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2023/day/7#part2">Challenge</a>
 */
public abstract class Day07Q2Tests extends TestBase
{

    /**
     * The day that we are solving the puzzle for.
     * eg: "Day 01"
     *
     * @return The day that we are solving the puzzle for.
     */
    @Override
    protected String getDayLabel()
    {
        return "Day 07 Q2";
    }

    /**
     * Gets the sample input that was provided by the puzzle question.
     *
     * @return The sample input that was provided by the puzzle question.
     */
    @Override
    protected String getSampleInput()
    {
        return """
                32T3K 765
                T55J5 684
                KK677 28
                KTJJT 220
                QQQJA 483
                """;
    }

    /**
     * Gets the sample answer that was provided by the puzzle question.
     *
     * @return The sample answer that was provided by the puzzle question.
     */
    @Override
    protected String getSampleAnswer()
    {
        return "5905";
    }

    /**
     * This tests that the {@link #solve(String) solution}
     * gets the {@link #getSampleAnswer() sample answer}
     * by using the {@link #getSampleInput() sample input}.
     */
    @Test
    @Override
    public void testSample()
    {
        super.testSample();
    }

    /**
     * This tests that the {@link #solve(String) solution}
     * gets the {@link #getActualAnswer() actual answer}
     * by using the {@link #getActualInput() actual input}.
     */
    @Test
    @Override
    public void testSolution() throws IOException
    {
        super.testSolution();
    }

    /**
     * Gets the actual answer that we compute as the solution.
     * Usually you leave this blank initially, let the test fail and then update it to what the solution produces from {@link #solve(String)}.
     *
     * @return The actual answer that we compute as the solution.
     */
    @Override
    protected String getActualAnswer()
    {
        return "250382098";
    }

    public static class Solution1Tests extends Day07Q2Tests
    {

        /**
         * This tests that the {@link #solve(String) solution}
         * gets the {@link #getSampleAnswer() sample answer}
         * by using the {@link #getSampleInput() sample input}.
         */
        @Test
        @Override
        public void testSample()
        {
            super.testSample();
        }

        public record Hand(List<Card> cards, int bid)
        {
            /**
             * This rates the hand of cards to get its strength.
             * @return The rated hand of cards.
             */
            public RatedHand rateCards()
            {
                int strength = 1;

                // Get the type of hand that we have:
                HandType handType = HandType.rateHandType(cards());

                return new RatedHand(cards(), bid(), handType, strength);
            }

        }
        public record RatedHand(List<Card> cards, int bid, HandType handType, int strength)
        {
            public Card firstCard() { return cards().get(0); }
            public Card secondCard() { return cards().get(1); }
            public Card thirdCard() { return cards().get(2); }
            public Card fourthCard() { return cards().get(3); }
            public Card fifthCard() { return cards().get(4); }
        }
        public record RankedHand(List<Card> cards, int bid, HandType handType, int strength, int rank) {}

        @Override
        public String solve(String input)
        {
            // Parse the hands:
            var ratedCards = Arrays.stream(input.split("\\n"))
                    .map(this::parseHand)
                    .map(Hand::rateCards)
                    .sorted(
                            Comparator
                                    .comparing(RatedHand::handType)
                                    .thenComparing(RatedHand::firstCard)
                                    .thenComparing(RatedHand::secondCard)
                                    .thenComparing(RatedHand::thirdCard)
                                    .thenComparing(RatedHand::fourthCard)
                                    .thenComparing(RatedHand::fifthCard)
                    )
                    .toList();

            // Rank the cards based on their strength:
            int runningRank = 1;
            List<RankedHand> rankedHands = new ArrayList<>();
            for (RatedHand ratedHand : ratedCards)
            {
                rankedHands.add(new RankedHand(ratedHand.cards(), ratedHand.bid(), ratedHand.handType(), ratedHand.strength(), runningRank++));
            }

            // Work out the total score:
            var result = rankedHands.stream()
                    .map(h -> h.bid() * h.rank())
                    .reduce(Integer::sum)
                    .get();

            return result.toString();

        }

        public enum Card
        {
            _J("Joker", 1  ),
            _2("2"    , 2  ),
            _3("3"    , 3  ),
            _4("4"    , 4  ),
            _5("5"    , 5  ),
            _6("6"    , 6  ),
            _7("7"    , 7  ),
            _8("8"    , 8  ),
            _9("9"    , 9  ),
            _T("Ten"  , 10 ),
            _Q("Queen", 12 ),
            _K("King" , 13 ),
            _A("Ace"  , 14 );

            final String cardName;

            final int cardValue;

            Card(String cardName, int cardValue)
            {
                this.cardName = cardName;
                this.cardValue = cardValue;
            }
        }


        public Hand parseHand(String input)
        {
            // Split the cards from the bid:
            var parts = input.split("\\s+");

            // Split the cards:
            var cardSplit = parts[0].split("");

            // Get the set of cards:
            List<Card> cards = Arrays.stream(cardSplit).map(s -> Card.valueOf("_" + s)).toList();

            // Get the bid:
            int bid = Integer.parseInt(parts[1]);

            // Create the hand of cards:
            var hand = new Hand(cards, bid);

            return hand;
        }

        public enum HandType
        {
            Normal,
            HighCard,
            OnePair,
            TwoPair,
            ThreeOfAKind,
            FullHouse,
            FourOfAKind,
            FiveOfAKind;

            /**
             * This rates the given cards as a hand type.
             * @param cards The cards to rate.
             * @return The type of hand that we rated.
             */
            public static HandType rateHandType(List<Card> cards)
            {
                // Create a set of the cards:
                Set<Card> cardSet = new TreeSet<>(cards);

                // Index the cards:
                Map<Card, Integer> cardToCountMap = new HashMap<>();
                TreeMap<Integer, List<Card>> countToCardMap = new TreeMap<>();
                indexCards(cards, cardToCountMap, countToCardMap);
                // Now we have the card counts.

                // Check whether we have jokers:
                Integer jokerCount = cardToCountMap.getOrDefault(_J, 0);

                // Figure out the hand type based on what we see:
                return switch (cardSet.size())
                {
                    case 1 -> FiveOfAKind; // This is the only option when they are all the same.
                    case 2 ->
                    {
                        // Check what the highest count is:
                        Map.Entry<Integer, List<Card>> highestEntry = countToCardMap.lastEntry();

                        yield switch (highestEntry.getKey())
                        {
                            case 4 -> {
                                yield switch (jokerCount)
                                {
                                    case 1, 2, 3, 4, 5 -> FiveOfAKind;
                                    default -> FourOfAKind;
                                };
                            }
                            case 3 -> {
                                yield switch (jokerCount)
                                {
                                    case 1 -> FourOfAKind;
                                    case 2, 3, 4, 5 -> FiveOfAKind;
                                    default -> FullHouse;
                                };
                            }
                            default -> Normal;
                        };
                    }
                    case 3 ->
                    {
                        // Check what the highest count is:
                        Map.Entry<Integer, List<Card>> highestEntry = countToCardMap.lastEntry();

                        yield switch (highestEntry.getKey())
                        {
                            case 2 ->
                            {
                                yield switch (jokerCount)
                                {
                                    case 1 -> FullHouse;
                                    case 2 -> FourOfAKind;
                                    case 3 -> FiveOfAKind;
                                    default -> TwoPair;
                                };
                            }
                            case 3 -> {
                                yield switch (jokerCount)
                                {
                                    case 1 -> FourOfAKind;
                                    case 2 -> FiveOfAKind;
                                    case 3 -> FourOfAKind;
                                    default -> ThreeOfAKind;
                                };
                            }
                            default -> Normal;
                        };
                    }
                    case 4 ->
                    {
                        // Check what the highest count is:
                        Map.Entry<Integer, List<Card>> highestEntry = countToCardMap.lastEntry();

                        yield switch (highestEntry.getKey())
                        {
                            case 2 ->{
                                yield switch (jokerCount)
                                {
                                    case 1, 2 -> ThreeOfAKind;
                                    case 3, 4, 5 -> FiveOfAKind;
                                    default -> OnePair;
                                };
                            }
                            default -> Normal;
                        };
                    }

                    case 5 -> {
                        yield switch (jokerCount)
                        {
                            case 1 -> OnePair;
                            case 2 -> ThreeOfAKind;
                            case 3 -> FourOfAKind;
                            case 4, 5 -> FiveOfAKind;
                            default -> HighCard;
                        };
                    }

                    default -> Normal;
                };
            }


            /**
             * Indexes the cards so that we have counts.
             * @param cards The cards to index.
             * @param cardToCountMap The card to count mapping.
             * @param countToCardMap The count to card mapping.
             */
            private static void indexCards(List<Card> cards,Map<Card, Integer> cardToCountMap, TreeMap<Integer, List<Card>> countToCardMap)
            {
                // Count how many of each card we have:
                for (Card card : cards)
                {
                    cardToCountMap.compute(card, (card1, integer) -> integer == null ? 1 : integer + 1);
                }
                // Now we have the card counts.

                // Index the counts:
                for (Map.Entry<Card, Integer> entry : cardToCountMap.entrySet())
                {
                    // Get the group of cards with this count:
                    List<Card> group = countToCardMap.computeIfAbsent(entry.getValue(), integer -> new ArrayList<>());

                    // Add the card to the group:
                    group.add(entry.getKey());
                }
            }

        }


        @Test
        public void testHandType_33332_FourOfAKind()
        {
            var cards = List.of(_3, _3, _3, _3, _2);
            var handType = HandType.rateHandType(cards);
            assertEquals(FourOfAKind, handType);
        }

        @Test
        public void testHandType_2AAAA_FourOfAKind()
        {
            var cards = List.of(_2, _A, _A, _A, _A);
            var handType = HandType.rateHandType(cards);
            assertEquals(FourOfAKind, handType);
        }

        @Test
        public void testHandType_77888_FullHouse()
        {
            var cards = List.of(_7, _7, _8, _8, _8);
            var handType = HandType.rateHandType(cards);
            assertEquals(FullHouse, handType);
        }

        @Test
        public void testHandType_77788_FullHouse()
        {
            var cards = List.of(_7, _7, _7, _8, _8);
            var handType = HandType.rateHandType(cards);
            assertEquals(FullHouse, handType);
        }

        @Test
        public void testHandType_T55J5_FourOfAKind()
        {
            var cards = List.of(_T, _5, _5, _J, _5);
            var handType = HandType.rateHandType(cards);
            assertEquals(FourOfAKind, handType);
        }

        @Test
        public void testHandType_QQQJA_FourOfAKind()
        {
            var cards = List.of(_Q, _Q, _Q, _J, _A);
            var handType = HandType.rateHandType(cards);
            assertEquals(FourOfAKind, handType);
        }

        @Test
        public void testHandType_KK677_TwoPair()
        {
            var cards = List.of(_K, _K, _6, _7, _7);
            var handType = HandType.rateHandType(cards);
            assertEquals(TwoPair, handType);
        }

        @Test
        public void testHandType_KTJJT_FourOfAKind()
        {
            var cards = List.of(_K, _T, _J, _J, _T);
            var handType = HandType.rateHandType(cards);
            assertEquals(FourOfAKind, handType);
        }

        @Test
        public void testHandType_32T3K_OnePair()
        {
            var cards = List.of(_3, _2, _T, _3, _K);
            var handType = HandType.rateHandType(cards);
            assertEquals(OnePair, handType);
        }

        @Test
        public void testHandType_AAAAA_FiveOfAKind()
        {
            var cards = List.of(_A, _A, _A, _A, _A);
            var handType = HandType.rateHandType(cards);
            assertEquals(FiveOfAKind, handType);
        }

        @Test
        public void testHandType_AA8AA_FourOfAKind()
        {
            var cards = List.of(_A, _A, _8, _A, _A);
            var handType = HandType.rateHandType(cards);
            assertEquals(FourOfAKind, handType);
        }

        @Test
        public void testHandType_23332_FullHouse()
        {
            var cards = List.of(_2, _3, _3, _3, _2);
            var handType = HandType.rateHandType(cards);
            assertEquals(FullHouse, handType);
        }

        @Test
        public void testHandType_TTT98_ThreeOfAKind()
        {
            var cards = List.of(_T, _T, _T, _9, _8);
            var handType = HandType.rateHandType(cards);
            assertEquals(ThreeOfAKind, handType);
        }

        @Test
        public void testHandType_23432_TwoPair()
        {
            var cards = List.of(_2, _3, _4, _3, _2);
            var handType = HandType.rateHandType(cards);
            assertEquals(TwoPair, handType);
        }

        @Test
        public void testHandType_23456_HighCard()
        {
            var cards = List.of(_2, _3, _4, _5, _6);
            var handType = HandType.rateHandType(cards);
            assertEquals(HighCard, handType);
        }

        @Test
        public void testHandType_QJJQ2_FourOfAKind()
        {
            var cards = List.of(_Q, _J, _J, _Q, _2);
            var handType = HandType.rateHandType(cards);
            assertEquals(FourOfAKind, handType);
        }

        @Test
        public void testHandType_JKKK2_FourOfAKind()
        {
            var cards = List.of(_J, _K, _K, _K, _2);
            var handType = HandType.rateHandType(cards);
            assertEquals(FourOfAKind, handType);
        }

        @Test
        public void testHandType_QQQQ2_FourOfAKind()
        {
            var cards = List.of(_Q, _Q, _Q, _Q, _2);
            var handType = HandType.rateHandType(cards);
            assertEquals(FourOfAKind, handType);
        }

        @Test
        public void testHandType_22JJJ_FiveOfAKind()
        {
            var cards = List.of(_2, _2, _J, _J, _J);
            var handType = HandType.rateHandType(cards);
            assertEquals(FiveOfAKind, handType);
        }

        @Test
        public void testHandType_JJ234_ThreeOfAKind()
        {
            var cards = List.of(_J, _J, _2, _3, _4);
            var handType = HandType.rateHandType(cards);
            assertEquals(ThreeOfAKind, handType);
        }

        @Test
        public void testHandType_AAKKJ_FullHouse()
        {
            var cards = List.of(_A, _A, _K, _K, _J);
            var handType = HandType.rateHandType(cards);
            assertEquals(FullHouse, handType);
        }

        @Test
        public void testHandType_234JJ_ThreeOfAKind()
        {
            var cards = List.of(_2, _3, _4, _J, _J);
            var handType = HandType.rateHandType(cards);
            assertEquals(ThreeOfAKind, handType);
        }

        @Test
        public void testHandType_2344J_ThreeOfAKind()
        {
            var cards = List.of(_2, _3, _4, _4, _J);
            var handType = HandType.rateHandType(cards);
            assertEquals(ThreeOfAKind, handType);
        }

        @Test
        public void testHandType_23444_ThreeOfAKind()
        {
            var cards = List.of(_2, _3, _4, _4, _4);
            var handType = HandType.rateHandType(cards);
            assertEquals(ThreeOfAKind, handType);
        }

        @Test
        public void testHandType_JJJJJ_FiveOfAKind()
        {
            var cards = List.of(_J, _J, _J, _J, _J);
            var handType = HandType.rateHandType(cards);
            assertEquals(FiveOfAKind, handType);
        }

        @Test
        public void testHandType_23JJJ_FourOfAKind()
        {
            var cards = List.of(_2, _3, _J, _J, _J);
            var handType = HandType.rateHandType(cards);
            assertEquals(FourOfAKind, handType);
        }
    }

}
