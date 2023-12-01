package io.nanovc.aoc2023.day01Q1;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * --- Day 1: Trebuchet?! ---
 * Something is wrong with global snow production, and you've been selected to take a look. The Elves have even given you a map; on it, they've used stars to mark the top fifty locations that are likely to be having problems.
 *
 * You've been doing this long enough to know that to restore snow operations, you need to check all fifty stars by December 25th.
 *
 * Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
 *
 * You try to ask why they can't just use a weather machine ("not powerful enough") and where they're even sending you ("the sky") and why your map looks mostly blank ("you sure ask a lot of questions") and hang on did you just say the sky ("of course, where do you think snow comes from") when you realize that the Elves are already loading you into a trebuchet ("please hold still, we need to strap you in").
 *
 * As they're making the final adjustments, they discover that their calibration document (your puzzle input) has been amended by a very young Elf who was apparently just excited to show off her art skills. Consequently, the Elves are having trouble reading the values on the document.
 *
 * The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover. On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.
 *
 * For example:
 *
 * 1abc2
 * pqr3stu8vwx
 * a1b2c3d4e5f
 * treb7uchet
 * In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.
 *
 * Consider your entire calibration document. What is the sum of all of the calibration values?
 * Website:
 * <a href="https://adventofcode.com/2023/day/1">Challenge</a>
 */
public class Day01Q1Tests extends TestBase
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
        return "Day 01 Q1";
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
                1abc2
                pqr3stu8vwx
                a1b2c3d4e5f
                treb7uchet
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
        return "142";
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
     * Solves the puzzle question with the given inputs.
     *
     * @param input The input data to use for the solution.
     * @return The solution to the puzzle question. Type this answer into the AOC answer box.
     */
    @Override
    public String solve(String input)
    {
        // Work through the file:
        try (Scanner scanner = new Scanner(input))
        {
            // Keep a running total:
            int total = 0;

            // Define the pattern to search for:
            var patternWithOneDigit = Pattern.compile("^[a-zA-Z]*(?<FirstDigit>\\d).*[a-zA-Z]*$");
            var patternWithLastDigit = Pattern.compile("^[a-zA-Z]*(?<FirstDigit>\\d).*[a-zA-Z]*(?<LastDigit>\\d)[a-zA-Z]*$");

            // Find the next pattern:
            while (scanner.hasNextLine())
            {
                // Get the next line:
                var line = scanner.nextLine();

                System.out.println("line = " + line);

                // Look for a match:
                int secretValue = 0;
                Matcher matcher = patternWithLastDigit.matcher(line);
                if (matcher.matches())
                {
                    // We found a match.

                    // Get the digits of interest:
                    var firstDigit = matcher.group("FirstDigit");
                    var lastDigit = matcher.group("LastDigit");

                    System.out.println("firstDigit = " + firstDigit);
                    System.out.println("lastDigit = " + lastDigit);

                    // Compile the secret value:
                    secretValue = Integer.parseInt(firstDigit + lastDigit);
                }
                else
                {
                    // Search for the other pattern:
                    matcher = patternWithOneDigit.matcher(line);
                    if (matcher.matches())
                    {
                        // Get the digits of interest:
                        var firstDigit = matcher.group("FirstDigit");

                        System.out.println("firstDigit = " + firstDigit);

                        // Compile the secret value:
                        secretValue = Integer.parseInt(firstDigit + firstDigit);
                    }
                }

                // Accumulate the total:
                total += secretValue;
            }
            return Integer.toString(total);
        }
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
        return "55712";
    }
}
