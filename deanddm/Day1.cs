using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2023
{
    internal class Day1
    {
        String[] lines = File.ReadAllLines("input.day1.txt");
        Dictionary<string, string> numbers = new Dictionary<string, string> { { "one", "o1e" }, { "two", "t2o" }, { "three", "t3e" }, { "four", "f4r" }, { "five", "f5v" }, { "six", "s6x" }, { "seven", "s7n" }, { "eight", "e8t" }, { "nine", "n9n" } };
        Func<string, int> numberExtractor = line => Int32.Parse($"{line.FirstOrDefault(x => Char.IsDigit(x))}{line.LastOrDefault(x => Char.IsDigit(x))}");

        internal void ExecutePart1()
        {
            int sum = lines.ToList().Sum(numberExtractor);
            Console.WriteLine("Day 1, Part 1:" + sum);
        }

        internal void ExecutePart2()
        {
            for (int i = 0; i < lines.Count(); i++)
            {
                foreach (var number in numbers)
                    lines[i] = lines[i].Replace(number.Key, number.Value);
            }

            int sum = lines.ToList().Sum(numberExtractor);
            Console.WriteLine("Day 1, Part 2:" + sum);
        }
    }
}
