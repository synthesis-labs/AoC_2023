using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2023
{
    internal class Day4
    {
        String[] lines = File.ReadAllLines("input.day4.txt");
        List<Game> games = new List<Game>();
        int score = 0;
        int totalTickets = 0;
        Dictionary<int, int> additionalCards = new Dictionary<int, int>();

        internal void ExecuteDay4Part1()
        {
            ParseInput();
            CalculatureWinningNumbers();

            Console.WriteLine("Day 4, Part 1: " + score);
        }

        internal void ExecuteDay4Part2()
        {
            CalculatureAdditionalTickets();
            Console.WriteLine("Day 4, Part 2: " + totalTickets);

        }

        private void ParseInput()
        {
            foreach (var line in lines)
            {
                var gameSplit = line.Split(':');
                var numbers = gameSplit[1].Split('|');

                games.Add(new Game
                {
                    CardNumber = Int32.Parse(gameSplit[0].Replace("Card ", "")),
                    WinningNumbers = numbers[0].Split(' ').Where(x => x != "").Select(Int32.Parse).ToList(),
                    ChosenNumbers = numbers[1].Split(' ').Where(x => x != "").Select(Int32.Parse).ToList()
                });
            }
        }

        private void CalculatureWinningNumbers()
        {
            score = games
                .Select(x => x.WinningNumbers.Intersect(x.ChosenNumbers))
                .Select(x => x.Aggregate(0, (acc, x) => acc == 0 ? 1 : acc + acc))
                .Sum();
        }

        private void CalculatureAdditionalTickets()
        {
            games.ForEach(x => additionalCards.Add(x.CardNumber, 1));

            for (int i = 0; i < games.Count; i++)
            {
                var game = games[i];
                var matchingNumbers = game.WinningNumbers.Intersect(game.ChosenNumbers).Count();

                for (int j = 0; j < additionalCards[game.CardNumber]; j++)
                    Enumerable.Range(game.CardNumber + 1, matchingNumbers).ToList().ForEach(x => additionalCards[x]++);
            }

            totalTickets = additionalCards.Values.Sum();
        }
    }

    internal class Game
    {
        internal int CardNumber { get; set; }
        internal List<int> WinningNumbers { get; set; } = new List<int>();
        internal List<int> ChosenNumbers { get; set; } = new List<int>();
    }
}
