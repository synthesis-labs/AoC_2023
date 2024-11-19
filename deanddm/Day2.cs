using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2023
{
    internal class Day2
    {
        String[] lines = File.ReadAllLines("input.day2.txt");
        Dictionary<string, int> gameLimits = new Dictionary<string, int> { { "red", 12 }, { "green", 13 }, { "blue", 14 } };

        internal void ExecuteDay1Part1()
        {
            int sumOfGameNumbers = 0;

            foreach (var line in lines)
            {
                var gameNumberResult = GetGameNumber(line);
                var sets = gameNumberResult.remainingText.Split(';');
                var passes = sets.ToList().Select(x => x.Split(',')).Select(x=> x.Select(y => y.Split(' '))).All(x=> x.All(z => Int32.Parse(z[1]) <= gameLimits[z[2]]));
                
                if (passes)
                    sumOfGameNumbers += gameNumberResult.gameNumber;
            }

            Console.WriteLine("Day 2, Part 1: " + sumOfGameNumbers);
        }

        internal void ExecuteDay1Part2()
        {
            int sumOfPowers = 0;

            foreach (var line in lines)
            {
                var gameNumberResult = GetGameNumber(line);
                var sets = gameNumberResult.remainingText.Split(';');
                int power = 1;

                foreach (var gameLimit in gameLimits)
                    power *= sets.ToList().Select(x => x.Split(',')).Select(x => x.Select(y => y.Split(' '))).SelectMany(x => x.Where(y => y[2] == gameLimit.Key)).Max(z => Int32.Parse(z[1]));

                sumOfPowers += power;
            }

            Console.WriteLine("Day 2, Part 2: " + sumOfPowers);
        }

        private (int gameNumber, string remainingText) GetGameNumber(string line)
        {
            var split = line.Split(':');
            int gameNumber = Int32.Parse(split[0].Replace("Game ", ""));
            return (gameNumber, split[1]);
        }
    }
}
