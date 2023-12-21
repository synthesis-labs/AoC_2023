package io.nanovc.aoc2023.day07q1;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

import static io.nanovc.aoc2023.day07q1.Day07Q1Tests.Solution1Tests.Card.*;
import static io.nanovc.aoc2023.day07q1.Day07Q1Tests.Solution1Tests.HandType.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * --- Day 7: Camel Cards ---
 * Your all-expenses-paid trip turns out to be a one-way, five-minute ride in an airship. (At least it's a cool airship!) It drops you off at the edge of a vast desert and descends back to Island Island.
 *
 * "Did you bring the parts?"
 *
 * You turn around to see an Elf completely covered in white clothing, wearing goggles, and riding a large camel.
 *
 * "Did you bring the parts?" she asks again, louder this time. You aren't sure what parts she's looking for; you're here to figure out why the sand stopped.
 *
 * "The parts! For the sand, yes! Come with me; I will show you." She beckons you onto the camel.
 *
 * After riding a bit across the sands of Desert Island, you can see what look like very large rocks covering half of the horizon. The Elf explains that the rocks are all along the part of Desert Island that is directly above Island Island, making it hard to even get there. Normally, they use big machines to move the rocks and filter the sand, but the machines have broken down because Desert Island recently stopped receiving the parts they need to fix the machines.
 *
 * You've already assumed it'll be your job to figure out why the parts stopped when she asks if you can help. You agree automatically.
 *
 * Because the journey will take a few days, she offers to teach you the game of Camel Cards. Camel Cards is sort of similar to poker except it's designed to be easier to play while riding a camel.
 *
 * In Camel Cards, you get a list of hands, and your goal is to order them based on the strength of each hand. A hand consists of five cards labeled one of A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2. The relative strength of each card follows this order, where A is the highest and 2 is the lowest.
 *
 * Every hand is exactly one type. From strongest to weakest, they are:
 *
 * Five of a kind, where all five cards have the same label: AAAAA
 * Four of a kind, where four cards have the same label and one card has a different label: AA8AA
 * Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
 * Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
 * Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
 * One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
 * High card, where all cards' labels are distinct: 23456
 * Hands are primarily ordered based on type; for example, every full house is stronger than any three of a kind.
 *
 * If two hands have the same type, a second ordering rule takes effect. Start by comparing the first card in each hand. If these cards are different, the hand with the stronger first card is considered stronger. If the first card in each hand have the same label, however, then move on to considering the second card in each hand. If they differ, the hand with the higher second card wins; otherwise, continue with the third card in each hand, then the fourth, then the fifth.
 *
 * So, 33332 and 2AAAA are both four of a kind hands, but 33332 is stronger because its first card is stronger. Similarly, 77888 and 77788 are both a full house, but 77888 is stronger because its third card is stronger (and both hands have the same first and second card).
 *
 * To play Camel Cards, you are given a list of hands and their corresponding bid (your puzzle input). For example:
 *
 * 32T3K 765
 * T55J5 684
 * KK677 28
 * KTJJT 220
 * QQQJA 483
 * This example shows five hands; each hand is followed by its bid amount. Each hand wins an amount equal to its bid multiplied by its rank, where the weakest hand gets rank 1, the second-weakest hand gets rank 2, and so on up to the strongest hand. Because there are five hands in this example, the strongest hand will have rank 5 and its bid will be multiplied by 5.
 *
 * So, the first step is to put the hands in order of strength:
 *
 * 32T3K is the only one pair and the other hands are all a stronger type, so it gets rank 1.
 * KK677 and KTJJT are both two pair. Their first cards both have the same label, but the second card of KK677 is stronger (K vs T), so KTJJT gets rank 2 and KK677 gets rank 3.
 * T55J5 and QQQJA are both three of a kind. QQQJA has a stronger first card, so it gets rank 5 and T55J5 gets rank 4.
 * Now, you can determine the total winnings of this set of hands by adding up the result of multiplying each hand's bid with its rank (765 * 1 + 220 * 2 + 28 * 3 + 684 * 4 + 483 * 5). So the total winnings in this example are 6440.
 *
 * Find the rank of every hand in your set. What are the total winnings?
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2023/day/7">Challenge</a>
 */
public abstract class Day07Q1Tests extends TestBase
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
        return "Day 07 Q1";
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
        return "6440";
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
        return "248569531";
    }

    public static class Solution1Tests extends Day07Q1Tests
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
            _2("2"   , 2  ),
            _3("3"   , 3  ),
            _4("4"   , 4  ),
            _5("5"   , 5  ),
            _6("6"   , 6  ),
            _7("7"   , 7  ),
            _8("8"   , 8  ),
            _9("9"   , 9  ),
            _T("Ten"  , 10 ),
            _J("Jack" , 11 ),
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
                            case 4 -> FourOfAKind;
                            case 3 -> FullHouse;
                            default -> Normal;
                        };
                    }
                    case 3 ->
                    {
                        // Check what the highest count is:
                        Map.Entry<Integer, List<Card>> highestEntry = countToCardMap.lastEntry();

                        yield switch (highestEntry.getKey())
                        {
                            case 2 -> TwoPair;
                            case 3 -> ThreeOfAKind;
                            default -> Normal;
                        };
                    }
                    case 4 ->
                    {
                        // Check what the highest count is:
                        Map.Entry<Integer, List<Card>> highestEntry = countToCardMap.lastEntry();

                        yield switch (highestEntry.getKey())
                        {
                            case 2 -> OnePair;
                            default -> Normal;
                        };
                    }

                    case 5 -> HighCard;

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
        public void testHandType_T55J5_ThreeOfAKind()
        {
            var cards = List.of(_T, _5, _5, _J, _5);
            var handType = HandType.rateHandType(cards);
            assertEquals(ThreeOfAKind, handType);
        }

        @Test
        public void testHandType_QQQQJA_ThreeOfAKind()
        {
            var cards = List.of(_Q, _Q, _Q, _J, _A);
            var handType = HandType.rateHandType(cards);
            assertEquals(ThreeOfAKind, handType);
        }

        @Test
        public void testHandType_KK677_TwoPair()
        {
            var cards = List.of(_K, _K, _6, _7, _7);
            var handType = HandType.rateHandType(cards);
            assertEquals(TwoPair, handType);
        }

        @Test
        public void testHandType_KTJJT7_TwoPair()
        {
            var cards = List.of(_K, _T, _J, _J, _T);
            var handType = HandType.rateHandType(cards);
            assertEquals(TwoPair, handType);
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

        /*
            HighCard,
            OnePair,
            TwoPair,
            ThreeOfAKind,
            FullHouse,
            FourOfAKind,
            FiveOfAKind;
         */
    }

}
