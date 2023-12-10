package io.nanovc.aoc2023.day10q1;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * --- Day 10: Pipe Maze ---
 * You use the hang glider to ride the hot air from Desert Island all the way up to the floating metal island. This island is surprisingly cold and there definitely aren't any thermals to glide on, so you leave your hang glider behind.
 *
 * You wander around for a while, but you don't find any people or animals. However, you do occasionally find signposts labeled "Hot Springs" pointing in a seemingly consistent direction; maybe you can find someone at the hot springs and ask them where the desert-machine parts are made.
 *
 * The landscape here is alien; even the flowers and trees are made of metal. As you stop to admire some metal grass, you notice something metallic scurry away in your peripheral vision and jump into a big pipe! It didn't look like any animal you've ever seen; if you want a better look, you'll need to get ahead of it.
 *
 * Scanning the area, you discover that the entire field you're standing on is densely packed with pipes; it was hard to tell at first because they're the same metallic silver color as the "ground". You make a quick sketch of all of the surface pipes you can see (your puzzle input).
 *
 * The pipes are arranged in a two-dimensional grid of tiles:
 *
 * | is a vertical pipe connecting north and south.
 * - is a horizontal pipe connecting east and west.
 * L is a 90-degree bend connecting north and east.
 * J is a 90-degree bend connecting north and west.
 * 7 is a 90-degree bend connecting south and west.
 * F is a 90-degree bend connecting south and east.
 * . is ground; there is no pipe in this tile.
 * S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
 * Based on the acoustics of the animal's scurrying, you're confident the pipe that contains the animal is one large, continuous loop.
 *
 * For example, here is a square loop of pipe:
 *
 * .....
 * .F-7.
 * .|.|.
 * .L-J.
 * .....
 * If the animal had entered this loop in the northwest corner, the sketch would instead look like this:
 *
 * .....
 * .S-7.
 * .|.|.
 * .L-J.
 * .....
 * In the above diagram, the S tile is still a 90-degree F bend: you can tell because of how the adjacent pipes connect to it.
 *
 * Unfortunately, there are also many pipes that aren't connected to the loop! This sketch shows the same loop as above:
 *
 * -L|F7
 * 7S-7|
 * L|7||
 * -L-J|
 * L|-JF
 * In the above diagram, you can still figure out which pipes form the main loop: they're the ones connected to S, pipes those pipes connect to, pipes those pipes connect to, and so on. Every pipe in the main loop connects to its two neighbors (including S, which will have exactly two pipes connecting to it, and which is assumed to connect back to those two pipes).
 *
 * Here is a sketch that contains a slightly more complex main loop:
 *
 * ..F7.
 * .FJ|.
 * SJ.L7
 * |F--J
 * LJ...
 * Here's the same example sketch with the extra, non-main-loop pipe tiles also shown:
 *
 * 7-F7-
 * .FJ|7
 * SJLL7
 * |F--J
 * LJ.LJ
 * If you want to get out ahead of the animal, you should find the tile in the loop that is farthest from the starting position. Because the animal is in the pipe, it doesn't make sense to measure this by direct distance. Instead, you need to find the tile that would take the longest number of steps along the loop to reach from the starting point - regardless of which way around the loop the animal went.
 *
 * In the first example with the square loop:
 *
 * .....
 * .S-7.
 * .|.|.
 * .L-J.
 * .....
 * You can count the distance each tile in the loop is from the starting point like this:
 *
 * .....
 * .012.
 * .1.3.
 * .234.
 * .....
 * In this example, the farthest point from the start is 4 steps away.
 *
 * Here's the more complex loop again:
 *
 * ..F7.
 * .FJ|.
 * SJ.L7
 * |F--J
 * LJ...
 * Here are the distances for each tile on that loop:
 *
 * ..45.
 * .236.
 * 01.78
 * 14567
 * 23...
 * Find the single giant loop starting at S. How many steps along the loop does it take to get from the starting position to the point farthest from the starting position?
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2023/day/10">Challenge</a>
 */
public abstract class Day10Q1Tests extends TestBase
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
        return "Day 10 Q1";
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
                ..F7.
                .FJ|.
                SJ.L7
                |F--J
                LJ...
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

    public static class Solution1Tests extends Day10Q1Tests
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

            // Parse the maze:
            Maze maze = parseMaze(input);

            // Create four walkers in each direction from the starting location:
            var walkers = new ArrayList<Walker>();
            var startingDirections = List.of(Direction.Up, Direction.Down, Direction.Left, Direction.Right);
            for (Direction startingDirection : startingDirections)
            {
                // Create the walker:
                Walker walker = new Walker();
                walkers.add(walker);
                walker.direction = Direction.Unknown;
                walker.nextDirection = startingDirection;
                walker.coordinate = maze.startingCoordinate;
                walker.distance = 0;
                walker.maze = maze;
            }

            // Keep track of walkers that we want to kill in each iteration:
            List<Walker> walkersToKill = new ArrayList<>();

            // Keep walking while we have walkers:
            outer: while (walkers.size() > 0)
            {
                // We have more walkers to walk.

                // Start walking:
                for (var walker : walkers)
                {
                    // Check whether the walker can take a next step:
                    if (walker.canWalk())
                    {
                        // The walker can walk.

                        // Let the walker take the next step:
                        walker.walk();

                        // Show the walk:
                        System.out.println(walker);
                    }
                    else
                    {
                        // The walker can't take the next step.
                        // Flag for the walker to be killed in the next round:
                        walkersToKill.add(walker);
                    }
                }

                // Kill walkers that can't keep moving:
                walkers.removeAll(walkersToKill);

                // Clear the list of walkers we want to kill so that we can gather more for this round:
                walkersToKill.clear();

                // Check if any of the walkers have collided into each other:
                for (Walker walker : walkers)
                {
                    // Check against the others:
                    for (Walker otherWalker : walkers)
                    {
                        if (walker == otherWalker) continue;

                        // Check if they have the same coordinates:
                        if (walker.coordinate.equals(otherWalker.coordinate))
                        {
                            // We have met another walker.

                            // Accumulate the distance walked:
                            result += walker.distance;

                            // Print out the walk:
                            System.out.println(walker.toStringWithDistance());

                            // Stop walking:
                            break outer;
                        }
                    }
                }
            }
            return "" + result;
        }

        public Maze parseMaze(String input)
        {
            // Split the input into lines:
            String[] lines = input.split("\\n");

            // Create the maze:
            Maze maze = new Maze();
            maze.tiles = new Tile[lines.length][lines[0].length()];

            // Go through each row:
            for (int i = 0; i < lines.length; i++)
            {
                // Get the line
                String line = lines[i];

                // Go through each tile of this line:
                for (int j = 0; j < line.length(); j++)
                {
                    // Parse each tile:
                    var symbol = line.substring(j, j+1);
                    var tile = Tile.parseTile(symbol);

                    // Save the symbol:
                    maze.tiles[i][j] = tile;

                    // Check if this is the starting location:
                    if (tile.equals(Tile.ST))
                    {
                        // This is the starting location.
                        maze.startingCoordinate = new Coordinate(i, j);
                    }
                }
            }

            return maze;
        }

        /**
         * The maze that we want to walk.
         */
        public static class Maze
        {
            /**
             * The tiles of the maze.
             */
            public Tile[][] tiles;

            /**
             * The starting coordinate in the maze.
             */
            public Coordinate startingCoordinate;

            public String toString()
            {
                StringBuilder stringBuilder = new StringBuilder();

                for (int i = 0; i < tiles.length; i++)
                {
                    // Get the row of the maze:
                    var row = tiles[i];

                    // Add a new line if necessary:
                    if (i > 0) stringBuilder.append("\n");

                    // Go through the row of the maze:
                    for (int j = 0; j < row.length; j++)
                    {
                        // Get the tile:
                        var tile = row[j];

                        // Print the tile:
                        stringBuilder.append(tile.prettySymbol);
                    }
                }

                return stringBuilder.toString();
            }

            /**
             * Getst the tile at the given coordiate of the maze.
             * @param row The index of the row that we want to access.
             * @param col The index of the column that we want to access.
             * @return The tile at that coordinate.
             */
            public Tile getTile(int row, int col)
            {
                if (row < 0 || row >= this.tiles.length) return Tile.XX;
                else
                {
                    var rowArray = this.tiles[row];
                    if (col < 0 || col >= rowArray.length) return Tile.XX;
                    else
                    {
                        return rowArray[col];
                    }
                }
            }

            /**
             * Getst the tile at the given coordiate of the maze.
             * @param coordinate The coordinate to sample in the maze.
             * @return The tile at that coordinate.
             */
            public Tile getTile(Coordinate coordinate)
            {
                return getTile(coordinate.row(), coordinate.col());
            }
        }

        /**
         * This knows how to walk around a {@link Maze}.
         */
        public static class Walker
        {
            /**
             * The direction that this walker is facing.
             */
            public Direction direction;

            /**
             * The next direction that this walker must walk.
             */
            public Direction nextDirection;

            /**
             * The coordinate of this walker in the maze.
             */
            public Coordinate coordinate;

            /**
             * The distance that the walker has walked so far.
             */
            public int distance;

            /**
             * The maze that this walker is walking in.
             */
            public Maze maze;

            public String toString()
            {
                if (maze == null) return "?";
                else
                {
                    StringBuilder sb = new StringBuilder();

                    for (int i = -2; i <= 2; i++)
                    {
                        for (int j = -2; j <= 2; j++)
                        {
                            // Get the coordinate of the maze that we want:
                            int row = this.coordinate.row() + i;
                            int col = this.coordinate.col() + j;

                            // Check if we are rendering the current walker:
                            if (i == 0 && j == 0)
                            {
                                // We are rendering the walker.

                                switch (this.direction)
                                {
                                    case Unknown ->
                                    {
                                        switch (this.nextDirection)
                                        {
                                            case Unknown -> sb.append("?");
                                            case Up -> sb.append("⭫");
                                            case Down -> sb.append("⭭");
                                            case Left -> sb.append("⭪");
                                            case Right -> sb.append("⭬");
                                        }
                                    }
                                    case Up ->
                                    {
                                        switch (this.nextDirection)
                                        {
                                            case Unknown -> sb.append("⭱");
                                            case Up -> sb.append("⮅");
                                            case Down -> sb.append("⮏");
                                            case Left -> sb.append("⮢");
                                            case Right -> sb.append("⮣");
                                        }
                                    }
                                    case Down ->
                                    {
                                        switch (this.nextDirection)
                                        {
                                            case Unknown -> sb.append("⭳");
                                            case Up -> sb.append("⮍");
                                            case Down -> sb.append("⮇");
                                            case Left -> sb.append("⮠");
                                            case Right -> sb.append("⮡");
                                        }
                                    }
                                    case Left ->
                                    {
                                        switch (this.nextDirection)
                                        {
                                            case Unknown -> sb.append("⭰");
                                            case Up -> sb.append("⮤");
                                            case Down -> sb.append("⮦");
                                            case Left -> sb.append("⮄");
                                            case Right -> sb.append("⮎");
                                        }
                                    }
                                    case Right ->
                                    {
                                        switch (this.nextDirection)
                                        {
                                            case Unknown -> sb.append("⭲");
                                            case Up -> sb.append("⮥");
                                            case Down -> sb.append("⮧");
                                            case Left -> sb.append("⮌");
                                            case Right -> sb.append("⮆");
                                        }
                                    }
                                }
                            }
                            else
                            {
                                // We are rendering the maze.

                                // Get the tile at the coordinate:
                                var tile = this.maze.getTile(row, col);

                                // Add the tile to the output:
                                sb.append(tile.prettySymbol);
                            }
                        }
                        sb.append("\n");
                    }

                    return sb.toString();
                }
            }

            public String toStringWithDistance()
            {
                if (maze == null) return "?";
                else
                {
                    StringBuilder sb = new StringBuilder();

                    for (int i = -2; i <= 2; i++)
                    {
                        for (int j = -2; j <= 2; j++)
                        {
                            // Get the coordinate of the maze that we want:
                            int row = this.coordinate.row() + i;
                            int col = this.coordinate.col() + j;

                            // Check if we are rendering the current walker:
                            if (i == 0 && j == 0)
                            {
                                // We are rendering the walker.
                                sb.append("*");
                            }
                            else
                            {
                                // We are rendering the maze.

                                // Get the tile at the coordinate:
                                var tile = this.maze.getTile(row, col);

                                // Add the tile to the output:
                                sb.append(tile.prettySymbol);
                            }
                        }
                        sb.append("\n");
                    }

                    sb.append("* = ").append(this.distance);

                    return sb.toString();
                }
            }

            /**
             * Checks whether the walker can take a step in the next direction.
             * @return True if the walker can take a step in the next direction. False if it can't.
             */
            public boolean canWalk()
            {
                // Get the current coordinate:
                var current = this.coordinate;

                // Get the current tile we are on:
                var tile = this.maze.getTile(this.coordinate);

                // Get the next coordinate:
                var next = nextCoordinate();

                // Check whether we can walk:
                return switch (tile)
                {
                    case NS -> next.equals(current.offset(-1,  0)) || next.equals(current.offset( 1, 0));
                    case EW -> next.equals(current.offset( 0, -1)) || next.equals(current.offset( 0, 1));
                    case NE -> next.equals(current.offset(-1,  0)) || next.equals(current.offset( 0, 1));
                    case NW -> next.equals(current.offset(-1,  0)) || next.equals(current.offset( 0,-1));
                    case SW -> next.equals(current.offset( 1,  0)) || next.equals(current.offset( 0,-1));
                    case SE -> next.equals(current.offset( 1,  0)) || next.equals(current.offset( 0, 1));
                    case ST ->
                    {
                        // Get the next tile:
                        var nextTile = this.maze.getTile(this.coordinate);

                        // Check whether we can walk that way:
                        yield switch (nextTile) { case NS, EW, NE, NW, SW, SE, ST -> true; default -> false; };
                    }
                    case XX, GR -> false;
                };


            }

            /**
             * Gets the next coordinate for the walker.
             * @return The next coordinate for the walker.
             */
            public Coordinate nextCoordinate()
            {
                // Get the current coordinates:
                int row = this.coordinate.row();
                int col = this.coordinate.col();

                // Get the coordinates of the next step:
                switch (this.nextDirection)
                {
                    case Up -> row--;
                    case Down -> row++;
                    case Left -> col--;
                    case Right -> col++;
                }

                return new Coordinate(row, col);
            }

            /**
             * This take a step in the next direction.
             */
            public void walk()
            {
                // Save the next direction as the current direction:
                this.direction = this.nextDirection;

                // Get the next coordinate for this walker:
                Coordinate nextCoordinate = nextCoordinate();

                // Update our distance:
                this.distance++;

                // Sample the maze at this new coordinate:
                Tile tile = this.maze.getTile(nextCoordinate);

                // Work out the next direction based on the pipe we are in:
                this.nextDirection = switch (tile)
                {
                    case NS -> switch (this.direction)
                    {
                        case Up -> Direction.Up;
                        case Down -> Direction.Down;
                        case Unknown, Left, Right -> Direction.Unknown;
                    };
                    case EW -> switch (this.direction)
                    {
                        case Left -> Direction.Left;
                        case Right -> Direction.Right;
                        case Unknown, Up, Down -> Direction.Unknown;
                    };
                    case NE -> switch (this.direction)
                    {
                        case Down -> Direction.Right;
                        case Left -> Direction.Up;
                        case Unknown, Up, Right -> Direction.Unknown;
                    };
                    case NW -> switch (this.direction)
                    {
                        case Down -> Direction.Left;
                        case Right -> Direction.Up;
                        case Unknown, Up, Left -> Direction.Unknown;
                    };
                    case SW -> switch (this.direction)
                    {
                        case Up -> Direction.Left;
                        case Right -> Direction.Down;
                        case Unknown, Down, Left -> Direction.Unknown;
                    };
                    case SE -> switch (this.direction)
                    {
                        case Up -> Direction.Right;
                        case Left -> Direction.Down;
                        case Unknown, Down, Right -> Direction.Unknown;
                    };
                    case GR, ST, XX -> Direction.Unknown;
                };

                // Set the next coordinate:
                this.coordinate = nextCoordinate;
            }

        }

        /**
         * The coordinate in the maze.
         * @param row The row index in the maze.
         * @param col The column index in the maze.
         */
        public record Coordinate(int row, int col)
        {
            /**
             * Offsets the current coordinate by the given amount.
             * @param rowDelta The amount to offset the row index.
             * @param colDelta The amount to offset the column index.
             * @return The new coordinate that was offset.
             */
            public Coordinate offset(int rowDelta, int colDelta)
            {
                return new Coordinate(row() + rowDelta, col() + colDelta);
            }
        };

        /**
         * This captures the direction of travel.
         */
        public enum Direction
        {
            Unknown,
            Up,
            Down,
            Left,
            Right,
        }

        /**
         * A tile in the maze.
         */
        public enum Tile
        {
            NS("|","║","| is a vertical pipe connecting north and south."),
            EW("-","═","- is a horizontal pipe connecting east and west."),
            NE("L","╚","L is a 90-degree bend connecting north and east."),
            NW("J","╝","J is a 90-degree bend connecting north and west."),
            SW("7","╗","7 is a 90-degree bend connecting south and west."),
            SE("F","╔","F is a 90-degree bend connecting south and east."),
            GR(".","░",". is ground; there is no pipe in this tile."),
            ST("S","╳","S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has."),
            XX(" ","⬞","We are outside of the maze.");

            Tile(String symbol, String prettySymbol, String description)
            {
                this.symbol = symbol;
                this.prettySymbol = prettySymbol;
                this.description = description;
            }

            /**
             * The symbol as defined by the input.
             */
            public final String symbol;

            /**
             * The symbol that is prettier to look at.
             */
            public final String prettySymbol;

            /**
             * The description of the tile.
             */
            public final String description;


            /**
             * This parses the tile from the given symbol.
             * @param symbol The symbol to parse into a tile.
             * @return The tile that corresponds to the symbol.
             */
            public static Tile parseTile(String symbol)
            {
                return switch (symbol)
                {
                    case "|" -> NS;
                    case "-" -> EW;
                    case "L" -> NE;
                    case "J" -> NW;
                    case "7" -> SW;
                    case "F" -> SE;
                    case "." -> GR;
                    case "S" -> ST;
                    default  -> XX;
                };
            }

            @Override
            public String toString()
            {
                return this.prettySymbol;
            }
        }

    }

}
