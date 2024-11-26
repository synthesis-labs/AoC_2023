using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2023
{
    internal class Day6
    {
        String[] lines = File.ReadAllLines("input.day6.txt");
        Dictionary<int, double> records = new Dictionary<int, double>();
        List<double> combinations = new List<double>();

        internal void ExecutePart1()
        {
            ParseInput();
            FindCombinations();

            double sum = combinations.Aggregate(1d, (acc, x) => acc * x);
            Console.WriteLine("Day 6, Part 1: " + sum);
        }

        internal void ExecutePart2()
        {
            var combos = FindTheSingleRaceCombinations();
            Console.WriteLine("Day 6, Part 2: " + combos);
        }

        private void ParseInput()
        {
            var timeSplit = lines[0].Split("Time: ");
            var times = timeSplit[1].Split(' ').Where(x=> x != "").Select(x=> Int32.Parse(x)).ToList();
            var distanceSplit = lines[1].Split("Distance: ");
            var distances = distanceSplit[1].Split(' ').Where(x => x != "").Select(x => Int32.Parse(x)).ToList();

            for (int i = 0; i < times.Count; i++)
                records.Add(times[i], distances[i]);
        }

        private void FindCombinations()
        {
            foreach (var record in records) 
                combinations.Add(CalculateCombo(record.Key, record.Value));
        }

        private int CalculateCombo(int time, double distance)
        {
            var combo = 0;
            for (int i = 1; i < time; i++)
            {
                double remainingTime = time - i;
                double distanceTravelled = remainingTime * i;

                if (distanceTravelled > distance)
                    combo++;
            }

            return combo;
        }

        private int FindTheSingleRaceCombinations()
        {
            var time = Int32.Parse(String.Join(' ', records.Keys.ToList()).Replace(" ", ""));
            var distance = Double.Parse(String.Join(' ', records.Values.ToList()).Replace(" ", ""));

            return CalculateCombo(time, distance);
        }
    }
}
