package io.nanovc.aoc2023.day10q2;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * --- Part Two ---
 * You quickly reach the farthest point of the loop, but the animal never emerges. Maybe its nest is within the area enclosed by the loop?
 *
 * To determine whether it's even worth taking the time to search for such a nest, you should calculate how many tiles are contained within the loop. For example:
 *
 * ...........
 * .S-------7.
 * .|F-----7|.
 * .||.....||.
 * .||.....||.
 * .|L-7.F-J|.
 * .|..|.|..|.
 * .L--J.L--J.
 * ...........
 * The above loop encloses merely four tiles - the two pairs of . in the southwest and southeast (marked I below). The middle . tiles (marked O below) are not in the loop. Here is the same loop again with those regions marked:
 *
 * ...........
 * .S-------7.
 * .|F-----7|.
 * .||OOOOO||.
 * .||OOOOO||.
 * .|L-7OF-J|.
 * .|II|O|II|.
 * .L--JOL--J.
 * .....O.....
 * In fact, there doesn't even need to be a full tile path to the outside for tiles to count as outside the loop - squeezing between pipes is also allowed! Here, I is still within the loop and O is still outside the loop:
 *
 * ..........
 * .S------7.
 * .|F----7|.
 * .||OOOO||.
 * .||OOOO||.
 * .|L-7F-J|.
 * .|II||II|.
 * .L--JL--J.
 * ..........
 * In both of the above examples, 4 tiles are enclosed by the loop.
 *
 * Here's a larger example:
 *
 * .F----7F7F7F7F-7....
 * .|F--7||||||||FJ....
 * .||.FJ||||||||L7....
 * FJL7L7LJLJ||LJ.L-7..
 * L--J.L7...LJS7F-7L7.
 * ....F-J..F7FJ|L7L7L7
 * ....L7.F7||L7|.L7L7|
 * .....|FJLJ|FJ|F7|.LJ
 * ....FJL-7.||.||||...
 * ....L---J.LJ.LJLJ...
 * The above sketch has many random bits of ground, some of which are in the loop (I) and some of which are outside it (O):
 *
 * OF----7F7F7F7F-7OOOO
 * O|F--7||||||||FJOOOO
 * O||OFJ||||||||L7OOOO
 * FJL7L7LJLJ||LJIL-7OO
 * L--JOL7IIILJS7F-7L7O
 * OOOOF-JIIF7FJ|L7L7L7
 * OOOOL7IF7||L7|IL7L7|
 * OOOOO|FJLJ|FJ|F7|OLJ
 * OOOOFJL-7O||O||||OOO
 * OOOOL---JOLJOLJLJOOO
 * In this larger example, 8 tiles are enclosed by the loop.
 *
 * Any tile that isn't part of the main loop can count as being enclosed by the loop. Here's another example with many bits of junk pipe lying around that aren't connected to the main loop at all:
 *
 * FF7FSF7F7F7F7F7F---7
 * L|LJ||||||||||||F--J
 * FL-7LJLJ||||||LJL-77
 * F--JF--7||LJLJ7F7FJ-
 * L---JF-JLJ.||-FJLJJ7
 * |F|F-JF---7F7-L7L|7|
 * |FFJF7L7F-JF7|JL---7
 * 7-L-JL7||F7|L7F-7F7|
 * L.L7LFJ|||||FJL7||LJ
 * L7JLJL-JLJLJL--JLJ.L
 * Here are just the tiles that are enclosed by the loop marked with I:
 *
 * FF7FSF7F7F7F7F7F---7
 * L|LJ||||||||||||F--J
 * FL-7LJLJ||||||LJL-77
 * F--JF--7||LJLJIF7FJ-
 * L---JF-JLJIIIIFJLJJ7
 * |F|F-JF---7IIIL7L|7|
 * |FFJF7L7F-JF7IIL---7
 * 7-L-JL7||F7|L7F-7F7|
 * L.L7LFJ|||||FJL7||LJ
 * L7JLJL-JLJLJL--JLJ.L
 * In this last example, 10 tiles are enclosed by the loop.
 *
 * Figure out whether you have time to search for the nest by calculating the area within the loop. How many tiles are enclosed by the loop?
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2023/day/10#part2">Challenge</a>
 */
public abstract class Day10Q2Tests extends TestBase
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
        return "Day 10 Q2";
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
                ...........
                .S-------7.
                .|F-----7|.
                .||.....||.
                .||.....||.
                .|L-7.F-J|.
                .|..|.|..|.
                .L--J.L--J.
                ...........
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
        return "4";
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

    public static class Solution1Tests extends Day10Q2Tests
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

        @Test
        public void testSample_1()
        {
            final var INPUT = """
                    ...........
                    .S-------7.
                    .|F-----7|.
                    .||.....||.
                    .||.....||.
                    .|L-7.F-J|.
                    .|..|.|..|.
                    .L--J.L--J.
                    ...........
                    """;
            final var ANSWER = "4";
            assertEquals(
                    ANSWER,
                    solve(INPUT),
                    "The sample answer didn't match what was provided as the answer by the question. Update your solution in solve() until it matches the sample answer."
            );
        }

        @Test
        public void testSample_2()
        {
            final var INPUT = """
                    ..........
                    .S------7.
                    .|F----7|.
                    .||....||.
                    .||....||.
                    .|L-7F-J|.
                    .|..||..|.
                    .L--JL--J.
                    ..........
                    """;
            final var ANSWER = "4";
            assertEquals(
                    ANSWER,
                    solve(INPUT),
                    "The sample answer didn't match what was provided as the answer by the question. Update your solution in solve() until it matches the sample answer."
            );
        }

        @Test
        public void testSample_3()
        {
            final var INPUT = """
                    .F----7F7F7F7F-7....
                    .|F--7||||||||FJ....
                    .||.FJ||||||||L7....
                    FJL7L7LJLJ||LJ.L-7..
                    L--J.L7...LJS7F-7L7.
                    ....F-J..F7FJ|L7L7L7
                    ....L7.F7||L7|.L7L7|
                    .....|FJLJ|FJ|F7|.LJ
                    ....FJL-7.||.||||...
                    ....L---J.LJ.LJLJ...
                    """;
            final var ANSWER = "8";
            assertEquals(
                    ANSWER,
                    solve(INPUT),
                    "The sample answer didn't match what was provided as the answer by the question. Update your solution in solve() until it matches the sample answer."
            );
        }

        @Test
        public void testSample_4()
        {
            final var INPUT = """
                    FF7FSF7F7F7F7F7F---7
                    L|LJ||||||||||||F--J
                    FL-7LJLJ||||||LJL-77
                    F--JF--7||LJLJ7F7FJ-
                    L---JF-JLJ.||-FJLJJ7
                    |F|F-JF---7F7-L7L|7|
                    |FFJF7L7F-JF7|JL---7
                    7-L-JL7||F7|L7F-7F7|
                    L.L7LFJ|||||FJL7||LJ
                    L7JLJL-JLJLJL--JLJ.L
                    """;
            final var ANSWER = "10";
            assertEquals(
                    ANSWER,
                    solve(INPUT),
                    "The sample answer didn't match what was provided as the answer by the question. Update your solution in solve() until it matches the sample answer."
            );
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

                            // Flag the current walker location as being walked:
                            maze.walkedTiles[walker.coordinate.row()][walker.coordinate.col()] = WalkedTile.Walked;

                            // Print out the walk:
                            System.out.println(walker.toStringWithDistance());

                            // Stop walking:
                            break outer;
                        }
                    }
                }
            }
            // Now we have walked the full length of the pipe.

            // Do a flood-fill from the outer edges to find all the tiles that are outside the pipe:
            List<FloodFiller> floodFillers = new ArrayList<>();

            // Seed the algorithm just outside of the maze:
            List<Coordinate> startingSeeds = new ArrayList<>();
            for (int i = 0; i < maze.height; i++)
            {
                startingSeeds.add(new Coordinate(i, -1));
                startingSeeds.add(new Coordinate(i, maze.width));
            }
            for (int i = 0; i < maze.width; i++)
            {
                startingSeeds.add(new Coordinate(-1, i));
                startingSeeds.add(new Coordinate(maze.height, i));
            }
            for (Coordinate startingSeed : startingSeeds)
            {
                // Create the flood filler:
                FloodFiller floodFiller = new FloodFiller();
                floodFillers.add(floodFiller);
                floodFiller.coordinate = startingSeed;
                floodFiller.maze = maze;
            }

            // Allow each of the flood fillers to fill the maze:
            List<FloodFiller> floodFillersToKill = new ArrayList<>();
            List<FloodFiller> floodFillersToAdd = new ArrayList<>();
            while (floodFillers.size() > 0)
            {
                // We still have flood fillers.

                // Go through each flood filler:
                for (FloodFiller floodFiller : floodFillers)
                {
                    // Spawn the next flood fillers with a 1 neighbourhood around the current one:
                    for (int i = -1; i <= 1; i++)
                    {
                        for (int j = -1; j <= 1; j++)
                        {
                            // Skip the current coordinate.
                            if (i==0 || j==0) continue;

                            // Get the next coordinate:
                            Coordinate nextCoordinate = new Coordinate(floodFiller.coordinate.row() + i, floodFiller.coordinate.col() + j);

                            // Sample the maze to see if it has already been walked:
                            WalkedTile nextWalkedTile = maze.getWalkedTile(nextCoordinate);

                            // Check whether we can extend the flood fill there:
                            if (nextWalkedTile.equals(WalkedTile.NotWalked))
                            {
                                // We have not walked the tile yet.

                                // Create a new flood filler:
                                FloodFiller nextFloodFiller = new FloodFiller();
                                nextFloodFiller.maze = maze;
                                nextFloodFiller.coordinate = nextCoordinate;

                                // Add the flood filler to the next round:
                                floodFillersToAdd.add(nextFloodFiller);
                            }
                        }
                    }

                }

                // Kill All the old flood fillers:
                floodFillers.clear();

                // Mark the locations of all the flood fillers to add:
                for (FloodFiller floodFiller : floodFillersToAdd)
                {
                    // Mark the location as an outside node:
                    Coordinate coordinate = floodFiller.coordinate;
                    maze.walkedTiles[coordinate.row()][coordinate.col()] = WalkedTile.Outside;

                    // Add this to the next round of flood fillers:
                    floodFillers.add(floodFiller);
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
            maze.height = lines.length;
            maze.width = lines[0].length();
            maze.tiles = new Tile[maze.height][maze.width];
            maze.walkedTiles = new WalkedTile[maze.height][maze.width];

            // Go through each row:
            for (int i = 0; i < maze.height; i++)
            {
                // Get the line
                String line = lines[i];

                // Go through each tile of this line:
                for (int j = 0; j < maze.width; j++)
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
             * The width of the maze.
             */
            public int width;

            /**
             * The height of the maze.
             */
            public int height;

            /**
             * The tiles of the maze.
             */
            public Tile[][] tiles;

            /**
             * The tiles of the maze that have been walked.
             */
            public WalkedTile[][] walkedTiles;

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

                        // Check whether we have walked the tile:
                        var walkedTile = this.walkedTiles[i][j];

                        // Check if it is a walked tile:
                        if (walkedTile == null || walkedTile.equals(WalkedTile.NotWalked))
                        {
                            // This tile hasn't been walked.

                            // Print the tile:
                            stringBuilder.append(tile.prettySymbol);
                        }
                        else
                        {
                            // This tile has been walked.

                            // Print the walked tile:
                            stringBuilder.append(walkedTile.prettySymbol);
                        }
                    }
                }

                return stringBuilder.toString();
            }

            /**
             * Gets the tile at the given coordinate of the maze.
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
             * Gets the tile at the given coordinate of the maze.
             * @param coordinate The coordinate to sample in the maze.
             * @return The tile at that coordinate.
             */
            public Tile getTile(Coordinate coordinate)
            {
                return getTile(coordinate.row(), coordinate.col());
            }

            /**
             * Gets the tile at the given coordinate of the maze.
             * @param row The index of the row that we want to access.
             * @param col The index of the column that we want to access.
             * @return The tile at that coordinate.
             */
            public WalkedTile getWalkedTile(int row, int col)
            {
                if (row < 0 || row >= this.height) return WalkedTile.NotInMaze;
                else
                {
                    var rowArray = this.walkedTiles[row];
                    if (col < 0 || col >= this.width) return WalkedTile.NotInMaze;
                    else
                    {
                        WalkedTile walkedTile = rowArray[col];
                        if (walkedTile == null) walkedTile = WalkedTile.NotWalked;
                        return walkedTile;
                    }
                }
            }

            /**
             * Gets the walked tile at the given coordinate of the maze.
             * @param coordinate The coordinate to sample in the maze.
             * @return The walked tile at that coordinate.
             */
            public WalkedTile getWalkedTile(Coordinate coordinate)
            {
                return getWalkedTile(coordinate.row(), coordinate.col());
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

                // Flag that we have walked the tile:
                this.maze.walkedTiles[this.coordinate.row()][this.coordinate.col()] = WalkedTile.Walked;

                // Set the next coordinate:
                this.coordinate = nextCoordinate;
            }

        }

        /**
         * This does a flood fill of tiles by walking around
         */
        public static class FloodFiller
        {
            /**
             * The current coordinate of this flood filler.
             */
            public Coordinate coordinate;

            /**
             * The maze that this flood filler is in.
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
                                // We are rendering the flood filler.

                                sb.append("#");
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

        public enum WalkedTile
        {
            NotWalked(" "),
            Walked("*"),
            Outside("O"),
            Inside("I"),
            NotInMaze("⬞");

            WalkedTile(String prettySymbol)
            {
                this.prettySymbol = prettySymbol;
            }

            public final String prettySymbol;
        }

    }

}
