using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2023
{
    internal class Day7
    {
        String[] lines = File.ReadAllLines("input.day7.txt");
        internal static Dictionary<char, int> weights = new Dictionary<char, int>();
        internal static Dictionary<char, int> weightsA = new Dictionary<char, int>()
        {
            { '2', 2 }, { '3', 3 }, { '4', 4 }, { '5', 5 }, { '6', 6 }, { '7', 7 }, { '8', 8 }, { '9', 9 },
            { 'T', 10 }, { 'J', 11 }, { 'Q', 12 }, { 'K', 13 }, { 'A', 14 }
        };
        internal static Dictionary<char, int> weightsB = new Dictionary<char, int>()
        {
            { 'J', 1 }, { '2', 2 }, { '3', 3 }, { '4', 4 }, { '5', 5 }, { '6', 6 }, { '7', 7 }, { '8', 8 }, { '9', 9 },
            { 'T', 10 }, { 'Q', 11 }, { 'K', 12 }, { 'A', 13 }
        };
        List<Hand> hands = new List<Hand>();
        int winnings = 0;

        internal void ExecutePart1()
        {
            weights = weightsA;
            ParseInput(false);
            CalculateBids();

            Console.WriteLine("Day 7, Part 1: " + winnings);
        }

        internal void ExecutePart2()
        {
            weights = weightsB;
            ParseInput(true);
            CalculateBids();

            Console.WriteLine("Day 7, Part 2: " + winnings);
        }

        private void ParseInput(bool includeJCount)
        {
            hands = lines
                .Select(x => x.Split(' '))
                .Select(x => new Hand()
                {
                    cards = x[0],
                    bid = Int32.Parse(x[1]),
                    pairs = x[0].GroupBy(x => x)
                                .Select(x => new { x.Key, Value = x.Count() })
                                .OrderByDescending(x => x.Value)
                                .ThenByDescending(x => weights[x.Key])
                                .ToDictionary(t => t.Key, t => t.Value),
                    includeJCount = includeJCount
                })
                .Select(x => new Hand()
                {
                    bid = x.bid,
                    pairs = x.pairs,
                    cards = x.cards,
                    handType = DetermineHandType(x.pairs, x.includeJCount)
                })
                .OrderByDescending(x => (int)x.handType)
                .ThenByDescending(x => x)
                .ToList();

            hands.Reverse();
        }

        private HandType DetermineHandType(Dictionary<char, int> pairs, bool includeJCount)
        {
            if (includeJCount && pairs.ContainsKey('J') && (pairs['J'] != 5))
            {
                var jCount = pairs['J'];
                pairs.Remove('J');
                var firstPair = pairs.ElementAt(0);
                pairs[firstPair.Key] += jCount;
            }

            if (pairs.Values.Contains(5))
                return HandType.FiveOfAkind;
            else if (pairs.Values.Contains(4))
                return HandType.FourOfAKind;
            else if (pairs.Values.Contains(3) && pairs.Values.Contains(2))
                return HandType.FullHouse;
            else if (pairs.Values.Contains(3))
                return HandType.ThreeOfAKind;
            else if (pairs.Values.Where(x => x == 2).Count() == 2)
                return HandType.TwoPair;
            else if (pairs.Values.Where(x => x == 2).Count() == 1)
                return HandType.OnePair;
            else
                return HandType.HighCard;
        }

        private void CalculateBids()
        {
            winnings = hands.Select((x, y) => x.bid * (y + 1)).Sum();
        }
    }

    internal class Hand : IComparable<Hand>
    {
        internal string cards { get; set; } = "";
        internal int bid { get; set; }
        internal Dictionary<char, int> pairs { get; set; } = new Dictionary<char, int>();
        internal HandType handType { get; set; }
        internal bool includeJCount { get; set; }

        public int CompareTo(Hand? other)
        {
            if (other == null) return 0;

            for (int i = 0; i < cards.Length; i++)
            {
                char letterA = cards[i];
                char letterB = other.cards[i];
                if (letterA == letterB) continue;

                int weightA = Day7.weights[letterA];
                int weightB = Day7.weights[letterB];

                if (weightA < weightB) return -1;
                if (weightA > weightB) return 1;
            }

            return 0;
        }
    }

    internal enum HandType
    {
        HighCard,
        OnePair,
        TwoPair,
        ThreeOfAKind,
        FullHouse,
        FourOfAKind,
        FiveOfAkind
    }
}
