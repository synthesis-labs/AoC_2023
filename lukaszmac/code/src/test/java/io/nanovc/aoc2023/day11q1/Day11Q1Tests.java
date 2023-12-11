package io.nanovc.aoc2023.day11q1;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * --- Day 11: Cosmic Expansion ---
 * You continue following signs for "Hot Springs" and eventually come across an observatory. The Elf within turns out to be a researcher studying cosmic expansion using the giant telescope here.
 *
 * He doesn't know anything about the missing machine parts; he's only visiting for this research project. However, he confirms that the hot springs are the next-closest area likely to have people; he'll even take you straight there once he's done with today's observation analysis.
 *
 * Maybe you can help him with the analysis to speed things up?
 *
 * The researcher has collected a bunch of data and compiled the data into a single giant image (your puzzle input). The image includes empty space (.) and galaxies (#). For example:
 *
 * ...#......
 * .......#..
 * #.........
 * ..........
 * ......#...
 * .#........
 * .........#
 * ..........
 * .......#..
 * #...#.....
 * The researcher is trying to figure out the sum of the lengths of the shortest path between every pair of galaxies. However, there's a catch: the universe expanded in the time it took the light from those galaxies to reach the observatory.
 *
 * Due to something involving gravitational effects, only some space expands. In fact, the result is that any rows or columns that contain no galaxies should all actually be twice as big.
 *
 * In the above example, three columns and two rows contain no galaxies:
 *
 *    v  v  v
 *  ...#......
 *  .......#..
 *  #.........
 * >..........<
 *  ......#...
 *  .#........
 *  .........#
 * >..........<
 *  .......#..
 *  #...#.....
 *    ^  ^  ^
 * These rows and columns need to be twice as big; the result of cosmic expansion therefore looks like this:
 *
 * ....#........
 * .........#...
 * #............
 * .............
 * .............
 * ........#....
 * .#...........
 * ............#
 * .............
 * .............
 * .........#...
 * #....#.......
 * Equipped with this expanded universe, the shortest path between every pair of galaxies can be found. It can help to assign every galaxy a unique number:
 *
 * ....1........
 * .........2...
 * 3............
 * .............
 * .............
 * ........4....
 * .5...........
 * ............6
 * .............
 * .............
 * .........7...
 * 8....9.......
 * In these 9 galaxies, there are 36 pairs. Only count each pair once; order within the pair doesn't matter. For each pair, find any shortest path between the two galaxies using only steps that move up, down, left, or right exactly one . or # at a time. (The shortest path between two galaxies is allowed to pass through another galaxy.)
 *
 * For example, here is one of the shortest paths between galaxies 5 and 9:
 *
 * ....1........
 * .........2...
 * 3............
 * .............
 * .............
 * ........4....
 * .5...........
 * .##.........6
 * ..##.........
 * ...##........
 * ....##...7...
 * 8....9.......
 * This path has length 9 because it takes a minimum of nine steps to get from galaxy 5 to galaxy 9 (the eight locations marked # plus the step onto galaxy 9 itself). Here are some other example shortest path lengths:
 *
 * Between galaxy 1 and galaxy 7: 15
 * Between galaxy 3 and galaxy 6: 17
 * Between galaxy 8 and galaxy 9: 5
 * In this example, after expanding the universe, the sum of the shortest path between all 36 pairs of galaxies is 374.
 *
 * Expand the universe, then find the length of the shortest path between every pair of galaxies. What is the sum of these lengths?
 * Website:
 * <a href="https://adventofcode.com/2023/day/11">Challenge</a>
 */
public abstract class Day11Q1Tests extends TestBase
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
        return "Day 11 Q1";
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
                ...#......
                .......#..
                #.........
                ..........
                ......#...
                .#........
                .........#
                ..........
                .......#..
                #...#.....
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
        return "374";
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
        return "6812";
    }

    public static class Solution1Tests extends Day11Q1Tests
    {
        @Test
        @Override
        public void testSample()
        {
            super.testSample();
        }

        @Test
        @Override
        public void testSolution() throws IOException
        {
            super.testSolution();
        }

        @Override
        public String solve(String input)
        {
            // Keep track of the result:
            var result = 0;

            // Parse the galaxy:
            var originalUniverse = parseUniverse(input);

            // Expand the universe:
            var universe = originalUniverse.expandUniverse();

            return "" + result;
        }

        private Universe parseUniverse(String input)
        {
            // Create the universe:
            Universe universe = new Universe();

            // Parse each line:
            String[] lines = input.split("\\n");
            for (int y = 0; y < lines.length; y++)
            {
                // Get the line:
                var line = lines[y];

                // Parse the characters:
                for (int x = 0; x < line.length(); x++)
                {
                    // Get the character:
                    var ch = line.charAt(x);

                    // Check whether it's a galaxy:
                    if (ch == '#')
                    {
                        // This is a galaxy.

                        // Create the galaxy:
                        var galaxy = new Galaxy(new Coordinate(x, y));

                        // Add the galaxy to the universe:
                        universe.addGalaxy(galaxy);
                    }
                }
            }

            // Set the size of the galaxy:
            universe.yHeight = lines.length;
            universe.xWidth = lines[0].length();

            return universe;
        }

        /**
         * A universe of galaxies.
         */
        public static class Universe
        {
            /**
             * The galaxies in the universe.
             */
            public List<Galaxy> galaxies = new ArrayList<>();

            /**
             * The index of galaxies for fast lookup.
             */
            public Index<Galaxy> galaxyIndex = new Index<>();

            /**
             * The width of the galaxy in the x dimension.
             */
            public int xWidth;

            /**
             * The height of the galaxy in the y dimension.
             */
            public int yHeight;

            /**
             * Adds and indexes the galaxy.
             * @param galaxy The galaxy to add.
             */
            public void addGalaxy(Galaxy galaxy)
            {
                this.galaxies.add(galaxy);
                this.galaxyIndex.add(galaxy);
            }

            public String toString()
            {
                StringBuilder sb = new StringBuilder();

                for (int y = 0; y < yHeight; y++)
                {
                    for (int x = 0; x < xWidth; x++)
                    {
                        // Check whether there is a galaxy at the given coordinate:
                        Galaxy galaxy = getStarAtCoordinate(x, y);

                        // Print out the tile:
                        if (galaxy != null)
                        {
                            // This is a galaxy.

                            // Add a galaxy symbol:
                            sb.append("#");
                        }
                        else
                        {
                            // This is not a galaxy.
                            sb.append(".");
                        }
                    }
                    sb.append("\n");
                }

                return sb.toString();
            }

            /**
             * Gets the galaxy at the given coordinate.
             * @param x The x coordinate to search in.
             * @param y The y coordinate to search in.
             * @return The galaxy at that coordinate. Null if there is no galaxy there.
             */
            public Galaxy getStarAtCoordinate(int x, int y)
            {
                return galaxyIndex.findAtCoordinate(x, y);
            }

            /**
             * Gets the galaxy at the given coordinate.
             * @param coordinate The coordinate to search at.
             * @return The galaxy at that coordinate. Null if there is no galaxy there.
             */
            public Galaxy getStarAtCoordinate(Coordinate coordinate)
            {
                return galaxyIndex.findAtCoordinate(coordinate);
            }

            /**
             * Expands the universe wherever there are gaps.
             * @return The expanded universe.
             */
            public Universe expandUniverse()
            {
                // Create the expanded universe:
                Universe expandedUniverse = new Universe();
                
                // Work out the offsets to apply for each dimension:
                List<Integer> xOffsets = new ArrayList<>();
                List<Integer> yOffsets = new ArrayList<>();
                
                // Go through all gaps in the current universe:
                for (int x = 0; x < this.xWidth; x++)
                {
                    // Check whether there are any stars in this coordinate:
                    if (galaxyIndex.hasAnyItemsInX(x))
                    {
                        // There are galaxies in this x value.
                        // Flag that the coordinates must stay:
                        xOffsets.add(0);
                    }
                    else
                    {
                        // There are no galaxies in this x value.
                        // Expand the space by adding an offset:
                        xOffsets.add(1);
                    }
                }

                // Go through all gaps in the current universe:
                for (int y = 0; y < this.yHeight; y++)
                {
                    // Check whether there are any stars in this coordinate:
                    if (galaxyIndex.hasAnyItemsInY(y))
                    {
                        // There are galaxies in this y value.
                        // Flag that the coordinates must stay:
                        yOffsets.add(0);
                    }
                    else
                    {
                        // There are no galaxies in this y value.
                        // Expand the space by adding an offset:
                        yOffsets.add(1);
                    }
                }
                // Now we have found all the gaps and worked out where we need to offset.

                // Go through the coordinates and create the new galaxies:
                int xCumulativeOffset = 0;
                int yCumulativeOffset = 0;
                for (int y = 0; y < yHeight; y++)
                {
                    // Get the y offset:
                    int yOffset = yOffsets.get(y);

                    // Accumulate the offset:
                    yCumulativeOffset += yOffset;

                    // Reset the x cumulative amount:
                    xCumulativeOffset = 0;

                    for (int x = 0; x < xWidth; x++)
                    {
                        // Get the x offset:
                        int xOffset = xOffsets.get(x);

                        // Accumulate the offset:
                        xCumulativeOffset += xOffset;

                        // Get the galaxy at the given coordinate:
                        var oldGalaxy = getStarAtCoordinate(x, y);

                        // Check if we have a galaxy:
                        if (oldGalaxy != null)
                        {
                            // We have an old galaxy at this location.

                            // Create the new galaxy:
                            var newGalaxy = new Galaxy(new Coordinate(x + xCumulativeOffset, y + yCumulativeOffset));

                            // Add the new galaxy:
                            expandedUniverse.addGalaxy(newGalaxy);
                        }
                    }
                }

                // Set the new size of the universe:
                expandedUniverse.xWidth = this.xWidth + xCumulativeOffset;
                expandedUniverse.yHeight = this.yHeight + yCumulativeOffset;

                return expandedUniverse;
            }
        }

        /**
         * A galaxy in the universe.
         * @param coordinate The coordinate of the galaxy.
         */
        public record Galaxy(
                Coordinate coordinate
        )
            implements Vector
        {

            /**
             * Gets the x-coordinate of this galaxy.
             * @return The x-coordinate of this galaxy.
             */
            @Override
            public int x()
            {
                return this.coordinate().x();
            }

            /**
             * Gets the y-coordinate of this galaxy.
             * @return The y-coordinate of this galaxy.
             */
            @Override
            public int y()
            {
                return this.coordinate().y();
            }

            /**
             * Returns the distance between the two galaxies.
             * @param otherGalaxy The other galaxy to measure the distance to.
             * @return The distance between the two galaxies.
             */
            public Offset distanceBetween(Galaxy otherGalaxy)
            {
                return new Offset(Math.abs(otherGalaxy.x() - x()), Math.abs(otherGalaxy.y() - y()));
            }
        }

        public interface Vector
        {
            int x();
            int y();
        }


        /**
         * A 2D coordinate.
         * @param x The x-coordinate.
         * @param y The y-coordinate.
         */
        public record Coordinate(int x, int y) implements Vector
        {

        }

        /**
         * A 2D offset (Delta).
         * @param x The x-amount of the offset.
         * @param y The y-amount of the offset.
         */
        public record Offset(int x, int y) implements Vector
        {
            /**
             * @return The distance of this offset.
             */
            public int distance()
            {
                return x() + y();
            }
        }

        /**
         * This indexes the coordinates.
         */
        public static class Index<TItem extends Vector>
        {
            /**
             * The map of items indexed by the x coordinate.
             */
            public Map<Integer, List<TItem>> xIndex = new HashMap<>();

            /**
             * The map of items indexed by the y coordinate.
             */
            public Map<Integer, List<TItem>> yIndex = new HashMap<>();

            /**
             * Adds the galaxy to the index.
             * @param galaxy The galaxy to add to the index.
             */
            public void add(TItem galaxy)
            {
                // Get the groups:
                var xGroup = this.xIndex.computeIfAbsent(galaxy.x(), k -> new ArrayList<>());
                var yGroup = this.yIndex.computeIfAbsent(galaxy.y(), k -> new ArrayList<>());

                // Add the galaxy:
                xGroup.add(galaxy);
                yGroup.add(galaxy);
            }

            /**
             * Gets the item at the given coordinate.
             * @param x The x coordinate to search in.
             * @param y The y coordinate to search in.
             * @return The item at that coordinate. Null if there is no item there.
             */
            public TItem findAtCoordinate(int x, int y)
            {
                // Check the x dimension:
                var xGroup = this.xIndex.get(x);
                if (xGroup == null) return null;

                // Check the y dimension:
                var yGroup = this.yIndex.get(y);
                if (yGroup == null) return null;

                // Now we know we have something.

                // Search through the shorter list:
                if (xGroup.size() <= yGroup.size())
                {
                    // The xGroup is smaller than the yGroup.

                    // Search for the item:
                    for (TItem xItem : xGroup)
                    {
                        if (xItem.y() == y) return xItem;
                    }
                    // If we get here then it wasn't found.
                }
                else
                {
                    // The yGroup is smaller than the xGroup.

                    // Search for the item:
                    for (TItem yItem : yGroup)
                    {
                        if (yItem.x() == x) return yItem;
                    }
                }
                // If we get here then it wasn't found.
                return null;
            }

            /**
             * Gets the galaxy at the given coordinate.
             * @param coordinate The coordinate to search at.
             * @return The galaxy at that coordinate. Null if there is no galaxy there.
             */
            public TItem findAtCoordinate(Coordinate coordinate)
            {
                return findAtCoordinate(coordinate.x(), coordinate.y());
            }

            /**
             * Checks whether we have any items at the given x coordinate.
             * @param x The x value to inspect.
             * @return True if there are items at this x index.
             */
            public boolean hasAnyItemsInX(int x)
            {
                return xIndex.containsKey(x);
            }

            /**
             * Checks whether we have any items at the given y coordinate.
             * @param y The y value to inspect.
             * @return True if there are items at this y index.
             */
            public boolean hasAnyItemsInY(int y)
            {
                return yIndex.containsKey(y);
            }
        }

        @Test
        public void testGalaxyDistances_5_9()
        {
            Galaxy g5 = new Galaxy(new Coordinate(1,6));
            Galaxy g9 = new Galaxy(new Coordinate(5,11));
            assertEquals(9, g5.distanceBetween(g9).distance());
        }

        @Test
        public void testGalaxyDistances_1_7()
        {
            Galaxy g1 = new Galaxy(new Coordinate(4,0));
            Galaxy g7 = new Galaxy(new Coordinate(9,10));
            assertEquals(15, g1.distanceBetween(g7).distance());
        }

        @Test
        public void testGalaxyDistances_3_6()
        {
            Galaxy g3 = new Galaxy(new Coordinate(0,2));
            Galaxy g6 = new Galaxy(new Coordinate(12,7));
            assertEquals(17, g3.distanceBetween(g6).distance());
        }

        @Test
        public void testGalaxyDistances_8_9()
        {
            Galaxy g8 = new Galaxy(new Coordinate(0,11));
            Galaxy g9 = new Galaxy(new Coordinate(5,11));
            assertEquals(5, g8.distanceBetween(g9).distance());
        }

    }

}
