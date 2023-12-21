package io.nanovc.aoc2023.day04q2;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * --- Part Two ---
 * Just as you're about to report your findings to the Elf, one of you realizes that the rules have actually been printed on the back of every card this whole time.
 *
 * There's no such thing as "points". Instead, scratchcards only cause you to win more scratchcards equal to the number of winning numbers you have.
 *
 * Specifically, you win copies of the scratchcards below the winning card equal to the number of matches. So, if card 10 were to have 5 matching numbers, you would win one copy each of cards 11, 12, 13, 14, and 15.
 *
 * Copies of scratchcards are scored like normal scratchcards and have the same card number as the card they copied. So, if you win a copy of card 10 and it has 5 matching numbers, it would then win a copy of the same cards that the original card 10 won: cards 11, 12, 13, 14, and 15. This process repeats until none of the copies cause you to win any more cards. (Cards will never make you copy a card past the end of the table.)
 *
 * This time, the above example goes differently:
 *
 * Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
 * Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
 * Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
 * Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
 * Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
 * Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
 * Card 1 has four matching numbers, so you win one copy each of the next four cards: cards 2, 3, 4, and 5.
 * Your original card 2 has two matching numbers, so you win one copy each of cards 3 and 4.
 * Your copy of card 2 also wins one copy each of cards 3 and 4.
 * Your four instances of card 3 (one original and three copies) have two matching numbers, so you win four copies each of cards 4 and 5.
 * Your eight instances of card 4 (one original and seven copies) have one matching number, so you win eight copies of card 5.
 * Your fourteen instances of card 5 (one original and thirteen copies) have no matching numbers and win no more cards.
 * Your one instance of card 6 (one original) has no matching numbers and wins no more cards.
 * Once all of the originals and copies have been processed, you end up with 1 instance of card 1, 2 instances of card 2, 4 instances of card 3, 8 instances of card 4, 14 instances of card 5, and 1 instance of card 6. In total, this example pile of scratchcards causes you to ultimately have 30 scratchcards!
 *
 * Process all of the original and copied scratchcards until no more scratchcards are won. Including the original set of scratchcards, how many total scratchcards do you end up with?
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2023/day/3#part2">Challenge</a>
 */
public abstract class Day04Q2Tests extends TestBase
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
        return "Day 04 Q2";
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
                Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
                Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
                Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
                Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
                Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
                Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
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
        return "30";
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
     * Gets the actual answer that we compute as the solution.
     * Usually you leave this blank initially, let the test fail and then update it to what the solution produces from {@link #solve(String)}.
     *
     * @return The actual answer that we compute as the solution.
     */
    @Override
    protected String getActualAnswer()
    {
        return "5923918";
    }

    public static class Solution1Tests extends Day04Q2Tests
    {

        public record CardStep1(String cardText, String numberText)
        {
            public int parseCardNumber()
            {
                // Get number:
                Pattern pattern = Pattern.compile("Card\\s+(\\d+)");
                Matcher matcher = pattern.matcher(this.cardText());
                if (matcher.matches())
                {
                    return Integer.parseInt(matcher.group(1));
                }
                else return 0;
            }
        }
        public record CardStep2(int cardNumber, String winningNumbers, String ourNumbers)
        {
            public Set<Integer> parseWinningNumbers()
            {
                return parseNumbers(this.winningNumbers());
            }

            public Set<Integer> parseOurNumbers()
            {
                return parseNumbers(this.ourNumbers());
            }

            public Set<Integer> parseNumbers(String input)
            {
                return Arrays.stream(input.split("\\s+"))
                        .filter(s -> !s.isEmpty())
                        .map(Integer::parseInt)
                        .collect(Collectors.toSet());
            }
        }
        public record CardStep3(int cardNumber, Set<Integer> winningNumbers, Set<Integer> ourNumbers)
        {
        }

        public record CardStep4(int cardNumber, Set<Integer> winningNumbers, Set<Integer> ourNumbers, int matchingNumbers, AtomicInteger cardCount)
        {
        }

        @Override
        public String solve(String input)
        {
            var result = Arrays.stream(input.split("\\n"))
                    .map(s -> s.split(":"))
                    .map(s -> new CardStep1(s[0], s[1]) )
                    .map(c -> { var cardTextSplit = c.numberText().split("\\|"); return new CardStep2(c.parseCardNumber(), cardTextSplit[0], cardTextSplit[1] ); })
                    .map(c -> new CardStep3(c.cardNumber(), c.parseWinningNumbers(), c.parseOurNumbers()) )
                    .map(c ->
                    {
                        var set = new HashSet<>(c.winningNumbers());
                        set.retainAll(c.ourNumbers());
                        int winningCount = set.size();
                        return new CardStep4(c.cardNumber(), c.winningNumbers(), c.ourNumbers(), winningCount, new AtomicInteger(1));
                    })
                    .toList();

            // Accumulate the card multiples:
            for (int i = 0; i < result.size(); i++)
            {
                // Get the card:
                CardStep4 card = result.get(i);

                // Check whether we have winning numbers:
                if (card.matchingNumbers() > 0)
                {
                    // We had matching numbers for this card.

                    // Accumulate copies of cards:
                    for (int j = 0, cardIndex = i + 1; j < card.matchingNumbers(); j++, cardIndex++)
                    {
                        if (cardIndex >= result.size()) break;

                        // Get the card to update:
                        CardStep4 cardToUpdate = result.get(cardIndex);

                        // Add the amounts:
                        cardToUpdate.cardCount.accumulateAndGet(card.cardCount.get(), Integer::sum);
                    }
                }
            }

            // Get the total count of cards:
            long total = 0;
            for (CardStep4 card : result)
            {
                total += card.cardCount.get();
            }

            return Long.toString(total);
        }

    }

}
