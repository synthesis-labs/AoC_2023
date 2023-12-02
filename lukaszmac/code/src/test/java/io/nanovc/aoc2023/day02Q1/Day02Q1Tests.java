package io.nanovc.aoc2023.day02Q1;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * --- Day 2: Cube Conundrum ---
 * You're launched high into the atmosphere! The apex of your trajectory just barely reaches the surface of a large island floating in the sky. You gently land in a fluffy pile of leaves. It's quite cold, but you don't see much snow. An Elf runs over to greet you.
 *
 * The Elf explains that you've arrived at Snow Island and apologizes for the lack of snow. He'll be happy to explain the situation, but it's a bit of a walk, so you have some time. They don't get many visitors up here; would you like to play a game in the meantime?
 *
 * As you walk, the Elf shows you a small bag and some cubes which are either red, green, or blue. Each time you play this game, he will hide a secret number of cubes of each color in the bag, and your goal is to figure out information about the number of cubes.
 *
 * To get information, once a bag has been loaded with cubes, the Elf will reach into the bag, grab a handful of random cubes, show them to you, and then put them back in the bag. He'll do this a few times per game.
 *
 * You play several games and record the information from each game (your puzzle input). Each game is listed with its ID number (like the 11 in Game 11: ...) followed by a semicolon-separated list of subsets of cubes that were revealed from the bag (like 3 red, 5 green, 4 blue).
 *
 * For example, the record of a few games might look like this:
 *
 * Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
 * Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
 * Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
 * Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
 * Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
 * In game 1, three sets of cubes are revealed from the bag (and then put back again). The first set is 3 blue cubes and 4 red cubes; the second set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.
 *
 * The Elf would first like to know which games would have been possible if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?
 *
 * In the example above, games 1, 2, and 5 would have been possible if the bag had been loaded with that configuration. However, game 3 would have been impossible because at one point the Elf showed you 20 red cubes at once; similarly, game 4 would also have been impossible because the Elf showed you 15 blue cubes at once. If you add up the IDs of the games that would have been possible, you get 8.
 *
 * Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs of those games?
 * Consider your entire calibration document. What is the sum of all of the calibration values?
 * Website:
 * <a href="https://adventofcode.com/2023/day/2">Challenge</a>
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
        return "Day 02 Q1";
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
        return "8";
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
        return "2416";
    }

    public static class Solution1Tests extends Day02Q1Tests
    {
        public record GameSplit1(String gameText, String valueText) {}
        public record GameSplit2(int gameNumber, String[] cubeSplitText) {}
        public record GameSplit3(int gameNumber, List<CubeSet> cubeSets) {}
        public record CubePull(int count, CubeColor color) {}

        public enum CubeColor { red, green, blue }

        public record CubeSet(Map<CubeColor, CubePull> pulls) {}

        @Override
        public String solve(String input)
        {


            var parts = input.split("\\n");
            Integer result = Arrays.stream(parts)
                    .map(s -> s.split(":"))
                    .map(s -> new GameSplit1(s[0], s[1]) )
                    .map(r -> new GameSplit1(r.gameText().replace("Game ", ""), r.valueText()) )
                    .map(r -> new GameSplit2(Integer.parseInt(r.gameText()), r.valueText().trim().split("\\s*;\\s*")) )
                    .map(r -> new GameSplit3(r.gameNumber(), Arrays.stream(r.cubeSplitText()).map(this::parseCubeSet).toList()  ))
                    .filter(this::isGamePossible)
                    .map(r -> r.gameNumber() )
                    .reduce(0, Integer::sum);

            return result.toString();
        }


        /**
         * This parses the text that describes how cubes were pulled.
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
         * This captures the filter criteria to define whether a game was possible based on the problem criteria.
         * @param game The game being checked.
         * @return True if the game was possible.
         */
        public boolean isGamePossible(GameSplit3 game)
        {
            // Check whether cube pulls are out of bounds:
            for (CubeSet cubeSet : game.cubeSets)
            {
                var pulls = cubeSet.pulls();
                if (
                        pulls.get(CubeColor.red).count() > 12 ||
                        pulls.get(CubeColor.green).count() > 13 ||
                        pulls.get(CubeColor.blue).count() > 14
                ) return false;
            }
            // If we get here then the game was ok.
            return true;
        }
    }

}
