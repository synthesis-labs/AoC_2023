using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2023
{
    internal class Day10
    {
        String[] lines = File.ReadAllLines("input.day10.example4.txt");
        char[,] map = new char[0, 0];
        int row, column = 0;
        char[] upCharacters = new char[] { '|', '7', 'F' };
        char[] rightCharacters = new char[] { '-', '7', 'J' };
        char[] downCharacters = new char[] { '|', 'L', 'J' };
        char[] leftCharacters = new char[] { '-', 'F', 'L' };
        List<Coords> allCoords = new List<Coords>();
        int steps = 1;
        int enclosedBlocks = 0;

        internal void ExecutePart1()
        {
            ParseInput();
            StartFromS();

            Console.WriteLine("Day 10, Part 1: " + steps);
        }

        internal void ExecutePart2()
        {
            PopulateFoundCoords();

            Console.WriteLine("Day 10, Part 2: unsolved");
        }

        private void PopulateFoundCoords()
        {
            foreach (var coord in allCoords)
            {
                map[coord.Row, coord.Column] = 'X';
            }

            //for (int i = 0; i < map.GetLength(0); i++) // GetLength(0) gives the number of rows
            //{
            //    for (int j = 0; j < map.GetLength(1); j++) // GetLength(1) gives the number of columns
            //    {
            //        Console.Write(map[i, j] + " ");
            //    }
            //    Console.WriteLine(); // New line for each row
            //}

            // Step 1: Identify non-enclosed dots using flood-fill from boundaries
            bool[,] visited = new bool[map.GetLength(0), map.GetLength(1)];
            int rows = map.GetLength(0);
            int cols = map.GetLength(1);

            // Flood-fill from the boundaries
            for (int i = 0; i < rows; i++)
            {
                FloodFill(map, visited, i, 0);         // Left boundary
                FloodFill(map, visited, i, cols - 1); // Right boundary
            }

            for (int j = 0; j < cols; j++)
            {
                FloodFill(map, visited, 0, j);        // Top boundary
                FloodFill(map, visited, rows - 1, j);// Bottom boundary
            }

            // Step 2: Collect all dots that are truly enclosed
            List<(int Row, int Col)> enclosedDots = new List<(int, int)>();
            for (int i = 0; i < rows; i++)
            {
                for (int j = 0; j < cols; j++)
                {
                    if (map[i, j] == '.' && !visited[i, j])
                    {
                        enclosedDots.Add((i, j));
                    }
                }
            }

            enclosedBlocks = enclosedDots.Count;

            // Output the results
            //Console.WriteLine("Truly enclosed dots:");
            //foreach (var dot in enclosedDots)
            //{
            //    Console.WriteLine($"Row: {dot.Row}, Column: {dot.Col}");
            //}
        }

        static void FloodFill(char[,] grid, bool[,] visited, int row, int col)
        {
            int rows = grid.GetLength(0);
            int cols = grid.GetLength(1);

            // Boundary conditions: Check if out of bounds or already visited
            if (row < 0 || col < 0 || row >= rows || col >= cols || visited[row, col] || grid[row, col] != '.')
            {
                return;
            }

            // Mark the current cell as visited
            visited[row, col] = true;

            // Recursively flood-fill in all 4 directions
            FloodFill(grid, visited, row - 1, col); // Up
            FloodFill(grid, visited, row + 1, col); // Down
            FloodFill(grid, visited, row, col - 1); // Left
            FloodFill(grid, visited, row, col + 1); // Right
        }

        private void ParseInput()
        {
            map = new char[lines.Length, lines[0].Length];

            for (int i = 0; i < lines.Length; i++)
            {
                for (int j = 0; j < lines[i].Length; j++)
                {
                    map[i, j] = lines[i][j];

                    if (row == 0 && lines[i][j] == 'S')
                    {
                        row = i;
                        column = j;
                    }
                }
            }
        }

        private void StartFromS()
        {
            var startingCoords = StartTheSearch(Direction.None, row, column);
            allCoords.AddRange(startingCoords);
            List<Coords> nextSetOfCoords = new List<Coords>();

            while (true)
            {
                foreach (var coord in startingCoords)
                {
                    var nextCoords = StartTheSearch(coord.Direction, coord.Row, coord.Column);
                    nextSetOfCoords.AddRange(nextCoords);
                    allCoords.AddRange(nextCoords);
                }

                startingCoords.Clear();
                startingCoords.AddRange(nextSetOfCoords);
                nextSetOfCoords.Clear();

                if (startingCoords.Count == 0)
                    break;

                steps++;
            }
        }

        private List<Coords> StartTheSearch(Direction previousDirection, int rowNumber, int columnNumber)
        {
            var coords = new List<Coords>();

            if (rowNumber - 1 >= 0)
            {
                var up = map[rowNumber - 1, columnNumber];
                if (up != '.' && upCharacters.Contains(up) && previousDirection != Direction.Down)
                {
                    var coord = new Coords() { Character = up, Row = rowNumber - 1, Column = columnNumber, Direction = Direction.Up };
                    if (HaventSeenBefore(coord))
                        coords.Add(coord);
                }
            }

            if (columnNumber + 1 <= map.GetLength(1) - 1)
            {
                var right = map[rowNumber, columnNumber + 1];
                if (right != '.' && rightCharacters.Contains(right) && previousDirection != Direction.Left)
                {
                    var coord = new Coords() { Character = right, Row = rowNumber, Column = columnNumber + 1, Direction = Direction.Right };
                    if (HaventSeenBefore(coord))
                        coords.Add(coord);
                }
            }

            if (rowNumber + 1 <= map.GetLength(0) - 1)
            {
                var down = map[rowNumber + 1, columnNumber];
                if (down != '.' && downCharacters.Contains(down) && previousDirection != Direction.Up)
                {
                    var coord = new Coords() { Character = down, Row = rowNumber + 1, Column = columnNumber, Direction = Direction.Down };
                    if (HaventSeenBefore(coord))
                        coords.Add(coord);
                }
            }

            if (columnNumber - 1 >= 0)
            {
                var left = map[rowNumber, columnNumber - 1];
                if (left != '.' && leftCharacters.Contains(left) && previousDirection != Direction.Right)
                {
                    var coord = new Coords() { Character = left, Row = rowNumber, Column = columnNumber - 1, Direction = Direction.Left };
                    if (HaventSeenBefore(coord))
                        coords.Add(coord);
                }
            }

            return coords;
        }

        private bool HaventSeenBefore(Coords coord)
        {
            var exists = allCoords.FirstOrDefault(x => x.Row == coord.Row && x.Column == coord.Column);

            if (exists != null)
                return false;
            else
                return true;
        }
    }

    internal class Coords
    {
        internal char Character { get; set; }
        internal int Row { get; set; }
        internal int Column { get; set; }
        internal Direction Direction { get; set; }
    }

    internal enum Direction
    {
        Left,
        Right,
        Up,
        Down,
        None
    }
}
