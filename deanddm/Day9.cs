using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2023
{
    internal class Day9
    {
        String[] lines = File.ReadAllLines("input.day9.txt");
        List<List<long>> startingSequences = new List<List<long>>();
        List<long> futures = new List<long>();
        List<long> pasts = new List<long>();

        internal void ExecutePart1()
        {
            ParseInput();
            AddToEachSequence();

            Console.WriteLine("Day 9, Part 1: " + futures.Sum());
        }

        internal void ExecutePart2()
        {
            Console.WriteLine("Day 9, Part 2: " + pasts.Sum());
        }

        private void ParseInput()
        {
            startingSequences = lines.Select(x => x.Split(' ')).Select((x) => x.Select(y => Int64.Parse(y)).ToList()).ToList();
        }

        private void AddToEachSequence()
        {
            foreach (var sequence in startingSequences)
            {
                List<List<long>> increments = new List<List<long>>();
                increments.Add(sequence);

                var increment = DetermineIncrements(sequence);
                increments.Add(increment);

                while (!increments.Last().All(x => x == 0))
                {
                    increment = DetermineIncrements(increment);
                    increments.Add(increment);
                }

                increments.Reverse();
                increments[0].Add(0);

                for (var i = 0; i < increments.Count; i++)
                {
                    if (increments.Count == i + 1) break;
                    increments[i + 1].Add(increments[i + 1].Last() + increments[i].Last());
                }

                futures.Add(increments.Last().Last());

                increments[0].Insert(0, 0);
                for (var i = 0; i < increments.Count; i++)
                {
                    if (increments.Count == i + 1) break;
                    increments[i + 1].Insert(0, (increments[i + 1].First() - increments[i].First()));
                }

                pasts.Add(increments.Last().First());
            }
        }

        private List<long> DetermineIncrements(List<long> sequence)
        {
            List<long> increments = new List<long>();
            for (var i = 0; i < sequence.Count; i++)
            {
                if (sequence.Count == i + 1) break;

                var increment = sequence[i + 1] - sequence[i];
                increments.Add(increment);
            }
            return increments;
        }
    }
}
