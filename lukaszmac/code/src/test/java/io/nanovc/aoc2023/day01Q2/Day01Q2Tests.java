package io.nanovc.aoc2023.day01Q2;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * --- Day 1: Trebuchet?! ---
 * Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".
 *
 * Equipped with this new information, you now need to find the real first and last digit on each line. For example:
 *
 * two1nine
 * eightwothree
 * abcone2threexyz
 * xtwone3four
 * 4nineeightseven2
 * zoneight234
 * 7pqrstsixteen
 * In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.
 *
 * What is the sum of all of the calibration values?
 *
 * Consider your entire calibration document. What is the sum of all of the calibration values?
 * Website:
 * <a href="https://adventofcode.com/2023/day/1#part2">Challenge</a>
 */
public class Day01Q2Tests extends TestBase
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
        return "Day 01 Q2";
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
                two1nine
                eightwothree
                abcone2threexyz
                xtwone3four
                4nineeightseven2
                zoneight234
                7pqrstsixteen
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
        return "281";
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

    protected String decodeWordsIntoNumbers(String line)
    {
        StringBuilder result = new StringBuilder();

        while (!line.isEmpty())
        {
            if      (line.startsWith("one"  )) { result.append("1"); }
            else if (line.startsWith("two"  )) { result.append("2"); }
            else if (line.startsWith("three")) { result.append("3"); }
            else if (line.startsWith("four" )) { result.append("4"); }
            else if (line.startsWith("five" )) { result.append("5"); }
            else if (line.startsWith("six"  )) { result.append("6"); }
            else if (line.startsWith("seven")) { result.append("7"); }
            else if (line.startsWith("eight")) { result.append("8"); }
            else if (line.startsWith("nine" )) { result.append("9"); }
            else { result.append(line.charAt(0));  };

            // Move to the next character:
            line = line.substring(1);
        }

        return result.toString();
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

                // Replaced line:
                var replacedLine = decodeWordsIntoNumbers(line);

                System.out.println("replacedLine = " + replacedLine);

                // Look for a match:
                int secretValue = 0;
                Matcher matcher = patternWithLastDigit.matcher(replacedLine);
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
                    matcher = patternWithOneDigit.matcher(replacedLine);
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
        return "55413";
    }
}
