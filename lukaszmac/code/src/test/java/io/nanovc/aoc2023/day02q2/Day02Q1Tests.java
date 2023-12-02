package io.nanovc.aoc2023.day02q2;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * --- Part Two ---
 * The Elf says they've stopped producing snow because they aren't getting any water! He isn't sure why the water stopped; however, he can show you how to get to the water source to check it out for yourself. It's just up ahead!
 * <p>
 * As you continue your walk, the Elf poses a second question: in each game you played, what is the fewest number of cubes of each color that could have been in the bag to make the game possible?
 * <p>
 * Again consider the example games from earlier:
 * <p>
 * Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
 * Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
 * Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
 * Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
 * Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
 * In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes. If any color had even one fewer cube, the game would have been impossible.
 * Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
 * Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
 * Game 4 required at least 14 red, 3 green, and 15 blue cubes.
 * Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.
 * The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied together. The power of the minimum set of cubes in game 1 is 48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up these five powers produces the sum 2286.
 * <p>
 * For each game, find the minimum set of cubes that must have been present. What is the sum of the power of these sets?
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2023/day/2#part2">Challenge</a>
 */
public abstract class Day02Q1Tests extends TestBase
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
        return "Day 02 Q2";
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
                Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
                Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
                Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
                Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
                Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
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
        return "2286";
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
        return "63307";
    }

    public static class Solution1Tests extends Day02Q1Tests
    {
        public record GameSplit1(String gameText, String valueText) { }

        public record GameSplit2(int gameNumber, String[] cubeSplitText) { }

        public record GameSplit3(int gameNumber, List<CubeSet> cubeSets) { }

        public record CubePull(int count, CubeColor color) { }

        public enum CubeColor {red, green, blue}

        public record CubeSet(Map<CubeColor, CubePull> pulls) { }

        @Override
        public String solve(String input)
        {
            var parts = input.split("\\n");
            Integer result = Arrays.stream(parts)
                    .map(s -> s.split(":"))
                    .map(s -> new GameSplit1(s[0], s[1]))
                    .map(r -> new GameSplit1(r.gameText().replace("Game ", ""), r.valueText()))
                    .map(r -> new GameSplit2(Integer.parseInt(r.gameText()), r.valueText().trim().split("\\s*;\\s*")))
                    .map(r -> new GameSplit3(r.gameNumber(), Arrays.stream(r.cubeSplitText()).map(this::parseCubeSet).toList()))
                    .map(this::calculateGamePower)
                    .reduce(0, Integer::sum);

            return result.toString();
        }


        /**
         * This parses the text that describes how cubes were pulled.
         *
         * @param cubeSplitText The text like 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
         * @return The details of the set of cube pulls.
         */
        public CubeSet parseCubeSet(String cubeSplitText)
        {
            List<CubePull> cubePulls = Arrays.stream(cubeSplitText.split("\\s*,\\s*"))
                    .map(s -> s.split(" "))
                    .map(s -> new CubePull(Integer.parseInt(s[0]), CubeColor.valueOf(s[1])))
                    .toList();

            // Create the cube set of pulls:
            var cubeSet = new CubeSet(new LinkedHashMap<>());

            // Initialise the cube set with defaults:
            for (CubeColor cubeColor : CubeColor.values())
            {
                cubeSet.pulls().put(cubeColor, new CubePull(0, cubeColor));
            }

            // Process the cube pulls:
            cubePulls.forEach(cubePull -> cubeSet.pulls().put(cubePull.color(), cubePull));

            return cubeSet;
        }

        /**
         * This calculates the power of each game.
         *
         * @param game The game being checked.
         * @return The power of the game.
         */
        public int calculateGamePower(GameSplit3 game)
        {
            // Initialise the maximum counts for each cube color:
            Map<CubeColor, Integer> maxCountPerCubeColor = new LinkedHashMap<>();
            for (CubeColor cubeColor : CubeColor.values())
            {
                maxCountPerCubeColor.put(cubeColor, 0);
            }

            // Find the maximum count for each cube color:
            for (CubeSet cubeSet : game.cubeSets)
            {
                var pulls = cubeSet.pulls();
                for (Map.Entry<CubeColor, CubePull> pullEntry : pulls.entrySet())
                {
                    // Check whether this entry exceeds the maximum:
                    maxCountPerCubeColor.compute(
                            pullEntry.getKey(),
                            (cubeColor, currentMax) ->
                                    pullEntry.getValue().count() > currentMax ? pullEntry.getValue().count() : currentMax
                    );
                }
            }

            // Get the power from each cube color:
            int power = 1;
            for (CubeColor cubeColor : CubeColor.values())
            {
                power *= maxCountPerCubeColor.get(cubeColor);
            }
            return power;
        }
    }

}
