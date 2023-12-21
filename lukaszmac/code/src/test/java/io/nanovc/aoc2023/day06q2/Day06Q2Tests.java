package io.nanovc.aoc2023.day06q2;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * --- Part Two ---
 * As the race is about to start, you realize the piece of paper with race times and record distances you got earlier actually just has very bad kerning. There's really only one race - ignore the spaces between the numbers on each line.
 *
 * So, the example from before:
 *
 * Time:      7  15   30
 * Distance:  9  40  200
 * ...now instead means this:
 *
 * Time:      71530
 * Distance:  940200
 * Now, you have to figure out how many ways there are to win this single race. In this example, the race lasts for 71530 milliseconds and the record distance you need to beat is 940200 millimeters. You could hold the button anywhere from 14 to 71516 milliseconds and beat the record, a total of 71503 ways!
 *
 * How many ways can you beat the record in this one much longer race?
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2023/day/6#part2">Challenge</a>
 */
public abstract class Day06Q2Tests extends TestBase
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
        return "Day 06 Q2";
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
                Time:      7  15   30
                Distance:  9  40  200
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
        return "71503";
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
        return "33149631";
    }

    public static class Solution1Tests extends Day06Q2Tests
    {
        public record RaceInfo(long time, long distance) {}
        public record RaceResult(RaceInfo race, double time, double distance, double speed) {}

        @Override
        public String solve(String input)
        {
            // Parse the input:
            String[] splitLines = input.split("\\n");
            var splitTimes = splitLines[0].split("Time:\\s+")[1].replaceAll("\\s+", "");
            var splitDistances = splitLines[1].split("Distance:\\s+")[1].replaceAll("\\s+", "");
            List<RaceInfo> races = new ArrayList<>();

            RaceInfo raceInfo = new RaceInfo(Long.parseLong(splitTimes), Long.parseLong(splitDistances));
            races.add(raceInfo);
            System.out.println("raceInfo = " + raceInfo);
            // Now we have the race info.

            // Run every race:
            List<RaceResult> results = new ArrayList<>();
            long racePermutations = 1;
            for (RaceInfo race : races)
            {
                long recordCount = 0;
                for (long buttonDuration = 1; buttonDuration < race.time(); buttonDuration++)
                {
                    double raceRecordDistance = race.distance;
                    double raceDuration = ((double)buttonDuration) + raceRecordDistance / ((double)buttonDuration);
                    if (raceDuration < race.time())
                    {
                        // We have beat the record.
                        recordCount++;

                        // Capture the race result:
                        //RaceResult raceResult = new RaceResult(race, raceDuration, raceRecordDistance, raceRecordDistance / raceDuration);
                        //results.add(raceResult);
                        //System.out.println("race = " + race + " button: " + buttonDuration + " total duration: " + raceDuration);
                    }
                }
                racePermutations *= recordCount;
            }

            return Long.toString(racePermutations);
        }

    }

}
