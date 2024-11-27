using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2023
{
    internal class Day8
    {
        String[] lines = File.ReadAllLines("input.day8.txt");
        Instruction[] instructions = new Instruction[0];
        Dictionary<string, Tuple<string, string>> map = new Dictionary<string, Tuple<string, string>>();

        internal void ExecutePart1()
        {
            ParseInput();

            long steps = FollowDirections();
            Console.WriteLine("Day 8, Part 1: " + steps);
        }

        internal void ExecutePart2()
        {
            long steps = FollowAllPaths();
            Console.WriteLine("Day 8, Part 2: " + steps);
        }

        private void ParseInput()
        {
            instructions = lines[0].Select(x => (Instruction)x).ToArray();

            foreach (var line in lines.Skip(2))
            {
                var split = line.Split(" = ");
                var key = split[0];
                var nextSteps = split[1].Replace("(", "").Replace(")", "").Split(", ");

                map.Add(key, new Tuple<string, string>(nextSteps[0], nextSteps[1]));
            }
        }

        private long FollowDirections()
        {
            var direction = map["AAA"];
            return GetSteps(direction, false);
        }

        private long GetSteps(Tuple<string, string> direction, bool endsWithZ)
        {
            var sequence = 0;
            long steps = 0;

            while (true)
            {
                steps++;
                var nextMap = GetNextMap(instructions[sequence], direction);

                if ((endsWithZ && nextMap.EndsWith('Z')) ||
                    (!endsWithZ && nextMap == "ZZZ"))
                    return steps;

                direction = map[nextMap];
                sequence++;

                if (sequence == instructions.Length)
                    sequence = 0;
            }
        }

        /* Credit to @francoisb */
        static long GCD(long x, long y)
        {
            if (y == 0) return x;
            else return GCD(y, x % y);
        }
        /* Credit to @francoisb */
        Func<List<long>, long> LCM = (List<long> steps) => steps.Aggregate((x, y) => x * y / GCD(x, y));

        private long FollowAllPaths()
        {
            List<Tuple<string, string>> paths = map.Where(x => x.Key.EndsWith('A')).Select(x => x.Value).ToList();
            var steps = paths.Select(x => GetSteps(x, true)).ToList();
            var least = LCM(steps);

            return least;
        }

        private string GetNextMap(Instruction instruction, Tuple<string, string> direction)
        {
            switch (instruction)
            {
                case Instruction.Left:
                    return direction.Item1;
                case Instruction.Right:
                    return direction.Item2;
                default:
                    return "";
            }
        }
    }

    internal enum Instruction
    {
        Left = 'L',
        Right = 'R'
    }
}
