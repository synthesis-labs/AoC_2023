package io.nanovc.aoc2023;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * A base class for our tests.
 * This is our template for solving the puzzle for each day.
 * Website:
 * <a href="https://adventofcode.com/2023/</a>
 */
public abstract class TestBase
{
    /**
     * The day that we are solving the puzzle for.
     * eg: "Day 01"
     *
     * @return The day that we are solving the puzzle for.
     */
    protected abstract String getDayLabel();

    /**
     * Gets the name of the puzzle we are solving that can be used as code.
     *
     * @return The name of the puzzle we are solving that can be used as code.
     */
    protected String getDayCodeName()
    {
        return getDayLabel().replace(" ", "");
    }

    /**
     * Gets the name of the folder that we are solving the puzzle for.
     *
     * @return The name of the folder that we are solving the puzzle for.
     */
    protected String getDayFolderName()
    {
        return getDayCodeName().toLowerCase(Locale.ROOT);
    }

    /**
     * Gets the path to the folder for the day.
     *
     * @return The path to the folder for the day.
     */
    protected Path getPathToDayFolder()
    {
        return Path.of(
                "src", "test", "java", "io", "nanovc", "aoc2023",
                getDayFolderName()
        );
    }

    /**
     * Gets the path to the input file for the day.
     *
     * @return The path to the input file for the day.
     */
    protected Path getPathToDayInputFile()
    {
        return getPathToDayFolder().resolve("input.txt");
    }

    /**
     * Gets the sample input that was provided by the puzzle question.
     *
     * @return The sample input that was provided by the puzzle question.
     */
    protected abstract String getSampleInput();

    /**
     * Gets the actual input that was provided by the puzzle question.
     *
     * @return The actual input that was provided by the puzzle question.
     */
    protected String getActualInput() throws IOException
    {
        //System.out.println(getPathToDayInputFile().normalize().toAbsolutePath().toString());
        return Files.readString(getPathToDayInputFile(), StandardCharsets.UTF_8);
    }

    /**
     * Gets the sample answer that was provided by the puzzle question.
     *
     * @return The sample answer that was provided by the puzzle question.
     */
    protected abstract String getSampleAnswer();

    /**
     * Solves the puzzle question with the given inputs.
     *
     * @param input The input data to use for the solution.
     * @return The solution to the puzzle question. Type this answer into the AOC answer box.
     */
    public abstract String solve(String input);

    /**
     * Gets the actual answer that we compute as the solution.
     * Usually you leave this blank initially, let the test fail and then update it to what the solution produces from {@link #solve(String)}.
     *
     * @return The actual answer that we compute as the solution.
     */
    protected abstract String getActualAnswer();

    /**
     * This tests that the {@link #solve(String) solution}
     * gets the {@link #getSampleAnswer() sample answer}
     * by using the {@link #getSampleInput() sample input}.
     */
    @Test
    public void testSample()
    {
        final var INPUT = getSampleInput();
        final var ANSWER = getSampleAnswer();
        assertEquals(
                ANSWER,
                solve(INPUT),
                "The sample answer didn't match what was provided as the answer by the question. Update your solution in solve() until it matches the sample answer."
        );
    }

    /**
     * This tests that the {@link #solve(String) solution}
     * gets the {@link #getActualAnswer() actual answer}
     * by using the {@link #getActualInput() actual input}.
     */
    @Test
    public void testSolution() throws IOException
    {
        final var INPUT = getActualInput();
        final var ANSWER = getActualAnswer();
        assertEquals(
                ANSWER,
                solve(INPUT),
                "The actual answer didn't match what we expected. Update your solution in solve() or update the answer value to match what solve() is producing."
        );
    }

}
