using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2023
{
    internal class Day11
    {
        String[] lines = File.ReadAllLines("input.day11.example.txt");
        List<string> expandedLines = new List<string>();

        internal void ExecutePart1()
        {
            ExpandUniverse();
            SetThePairs();

            Console.WriteLine("Day 11, Part 1:");
        }

        private void ExpandUniverse()
        {
            for (int i = 0; i < lines.Length; i++)
            {
                expandedLines.Add(lines[i]);
                if (lines[i].All(x=> x == '.'))
                    expandedLines.Add(lines[i]);
            }
        }

        private void SetThePairs()
        {
            int sequence = 1;
            for (int i = 0; i < expandedLines.Count; i++)
            {
                for (int j = 0; j < expandedLines[i].Length; i++)
                {
                    if (expandedLines[i][j] == '#')
                    {
                        //expandedLines[i][j] = (char)sequence;
                        sequence++;
                    }
                }
            }
        }
    }
}
