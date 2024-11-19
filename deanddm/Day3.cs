using System;
using System.Collections.Generic;
using System.ComponentModel.Design;
using System.IO.MemoryMappedFiles;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2023
{
    internal class Day3
    {
        String[] lines = File.ReadAllLines("input.day3.txt");
        List<Map> numberList = new List<Map>();
        Dictionary<int, List<int>> symbols = new Dictionary<int, List<int>>();
        List<Gear> gears = new List<Gear>();
        List<int> partNumbers = new List<int>();
        int sumOfGearRatios = 0;

        internal void ExecuteDay3Part1()
        {
            GetPartNumbersAndSymbolsFromInput();
            FindEachNumberThatHasAnAdjacentSymbol();

            Console.WriteLine("Day 3, Part 1: " + partNumbers.Sum());
        }

        internal void ExecuteDay3Part2()
        {
            FindEachGearThatHasAdjacentNumbers();
            Console.WriteLine("Day 3, Part 2: " + sumOfGearRatios);
        }

        private void GetPartNumbersAndSymbolsFromInput()
        {
            for (int i = 0; i < lines.Length; i++)
            {
                string line = lines[i];
                string number = "";
                for (int j = 0; j < line.Length; j++)
                {
                    char c = line[j];
                    if (Char.IsDigit(c))
                        number += c;
                    else
                    {
                        if (number.Length > 0)
                        {
                            numberList.Add(new Map
                            {
                                number = Int32.Parse(number),
                                indexes = Enumerable.Range(j - number.Length, number.Length).ToList(),
                                line = i
                            });
                        }

                        number = "";
                    }

                    if (c != '.' && !char.IsLetterOrDigit(c))
                    {
                        if (!symbols.ContainsKey(i))
                            symbols.Add(i, new List<int>() { j });
                        else
                            symbols[i].Add(j);
                    }

                    if (c == '*')
                    {
                        gears.Add(new Gear() { line = i, index = j });
                    }
                }

                //This is necessary for the last number in the line
                if (number.Length > 0)
                {
                    numberList.Add(new Map
                    {
                        number = Int32.Parse(number),
                        indexes = Enumerable.Range(line.Length - number.Length, number.Length).ToList(),
                        line = i
                    });
                }
            }
        }

        private void FindEachNumberThatHasAnAdjacentSymbol()
        {
            foreach (var map in numberList)
            {
                var lineRange = new List<int>(map.indexes);

                if (lineRange.Min() - 1 >= 0)
                    lineRange.Insert(0, lineRange.Min() - 1);

                lineRange.Add(lineRange.Max() + 1);

                if ((symbols.ContainsKey(map.line - 1) && symbols[map.line - 1].Intersect(lineRange).Count() > 0) ||
                    (symbols.ContainsKey(map.line) && symbols[map.line].Intersect(lineRange).Count() > 0) ||
                    (symbols.ContainsKey(map.line + 1) && symbols[map.line + 1].Intersect(lineRange).Count() > 0))
                {
                    partNumbers.Add(map.number);
                    continue;
                }
            }
        }

        private void FindEachGearThatHasAdjacentNumbers()
        {
            foreach (var gear in gears)
            {
                var lineRange = new[] { gear.line - 1, gear.line, gear.line + 1 };
                var gearRange = new[] { gear.index - 1, gear.index, gear.index + 1 };

                var numbers = numberList.Where(x=> lineRange.Contains(x.line) && x.indexes.Intersect(gearRange).Count() > 0).ToList();

                if (numbers.Count == 2)
                    sumOfGearRatios += (numbers[0].number * numbers[1].number);
            }
        }
    }

    internal class Map
    {
        internal int number { get; set; }
        internal List<int> indexes { get; set; } = new List<int>();
        internal int line { get; set; }
    }

    internal class Gear
    {
        internal int line { get; set; }
        internal int index { get; set; }
    }
}
