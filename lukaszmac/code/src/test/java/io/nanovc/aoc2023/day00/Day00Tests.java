package io.nanovc.aoc2023.day00;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.NavigableSet;
import java.util.Scanner;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * --- Day 1: Calorie Counting ---
 * Santa's reindeer typically eat regular reindeer food, but they need a lot of magical energy to deliver presents on Christmas. For that, their favorite snack is a special type of star fruit that only grows deep in the jungle. The Elves have brought you on their annual expedition to the grove where the fruit grows.
 * <p>
 * To supply enough magical energy, the expedition needs to retrieve a minimum of fifty stars by December 25th. Although the Elves assure you that the grove has plenty of fruit, you decide to grab any fruit you see along the way, just in case.
 * <p>
 * Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
 * <p>
 * The jungle must be too overgrown and difficult to navigate in vehicles or access from the air; the Elves' expedition traditionally goes on foot. As your boats approach land, the Elves begin taking inventory of their supplies. One important consideration is food - in particular, the number of Calories each Elf is carrying (your puzzle input).
 * <p>
 * The Elves take turns writing down the number of Calories contained by the various meals, snacks, rations, etc. that they've brought with them, one item per line. Each Elf separates their own inventory from the previous Elf's inventory (if any) by a blank line.
 * <p>
 * For example, suppose the Elves finish writing their items' Calories and end up with the following list:
 * <p>
 * 1000
 * 2000
 * 3000
 * <p>
 * 4000
 * <p>
 * 5000
 * 6000
 * <p>
 * 7000
 * 8000
 * 9000
 * <p>
 * 10000
 * This list represents the Calories of the food carried by five Elves:
 * <p>
 * The first Elf is carrying food with 1000, 2000, and 3000 Calories, a total of 6000 Calories.
 * The second Elf is carrying one food item with 4000 Calories.
 * The third Elf is carrying food with 5000 and 6000 Calories, a total of 11000 Calories.
 * The fourth Elf is carrying food with 7000, 8000, and 9000 Calories, a total of 24000 Calories.
 * The fifth Elf is carrying one food item with 10000 Calories.
 * In case the Elves get hungry and need extra snacks, they need to know which Elf to ask:
 * they'd like to know how many Calories are being carried by the Elf carrying the most Calories.
 * In the example above, this is 24000 (carried by the fourth Elf).
 * <p>
 * Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2022/day/1">Challenge</a>
 */
public class Day00Tests extends TestBase
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
        return "Day 00";
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
                1000
                2000
                3000
                                
                4000
                                
                5000
                6000
                                
                7000
                8000
                9000
                                
                10000
                """;
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
     * Gets the sample answer that was provided by the puzzle question.
     *
     * @return The sample answer that was provided by the puzzle question.
     */
    @Override
    protected String getSampleAnswer()
    {
        return "24000";
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
        // Keep a stack of the highest values:
        TreeSet<Long> sortedTotals = new TreeSet<>();

        // Keep a running total:
        long total = 0;

        // Work through the file:
        try (Scanner scanner = new Scanner(input).useDelimiter("\\s"))
        {
            // Go through each line:
            while (scanner.hasNextLine())
            {
                // Check whether we have a number or a new line:
                if (scanner.hasNextLong())
                {
                    // We have a number to read.
                    long nextAmount = scanner.nextLong();

                    // Accumulate the total:
                    total += nextAmount;

                    System.out.println("nextAmount = " + nextAmount);
                }
                else
                {
                    // There is no number to read.
                    // This is the end of the group.
                    System.out.println("End of group. Total: " + total);

                    // Save the total:
                    sortedTotals.add(total);

                    // Reset the total:
                    total = 0;

                    // Read the next line so that we can go on:
                    scanner.nextLine();
                }
            }
        }
        // Now we have worked through all the totals.

        // Get the largest total:
        Long largestTotal = sortedTotals.getLast();

        System.out.println("largestTotal = " + largestTotal);

        return largestTotal.toString();
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
        return "68802";
    }
}
