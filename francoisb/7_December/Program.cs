var GetInput = (string inputFileName) =>
{
    List<string> result = new List<string>();
    StreamReader sr = new StreamReader(inputFileName);
    var line = sr.ReadLine();
    while (line != null)
    {
        result.Add(line);
        line = sr.ReadLine();
    }
    sr.Close();
    return result;
};

var PrintOutput = (string result) => Console.WriteLine(result);

var PrintProgress = (string Thread, long Count, long Start, long Remaining, long pos) =>
{
    lock (LockMe._sync)
    {
        Console.SetCursorPosition(0, Convert.ToInt32(pos));
        var percentage = (Convert.ToDecimal(Count) - Convert.ToDecimal(Start)) / (Convert.ToDecimal(Remaining) - Convert.ToDecimal(Start)) * 100;
        Console.WriteLine($"{Thread}: {Count} / {Remaining} {Math.Round(percentage, 2)} %");
    }
};

var GenerateCollections = (List<string> lines) =>
{
    var res = new List<Map>();
    var res2 = new List<Map2>();
    foreach (var line in lines)
    {
        var temp = line.Split(' ');
        var hand = temp[0];
        var bid = long.Parse(temp[1]);
        res.Add(new Map(hand, bid, new List<char>() { 'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2' }));
        res2.Add(new Map2(hand, bid, new List<char>() { 'A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J' }));
    }
    
    return new Collection(res, res2);
};

var ProcessInputStep1 = (List<string> lines) =>
{
    var collection = GenerateCollections(lines);

    long res = 0;

    var sortedHands = collection.Hands.Order().ToList();

    for(var i = 0; i < sortedHands.Count(); i++)
    {
        res += sortedHands[i].Bid * (i+1);
    }

    return res;
};

var ProcessInputStep2 = (List<string> lines) =>
{
    var collection = GenerateCollections(lines);

    long res = 0;

    var sortedHands = collection.Hands2.Order().ToList();

    for (var i = 0; i < sortedHands.Count(); i++)
    {
        res += sortedHands[i].Bid * (i + 1);
    }

    return res;
};

var input = GetInput("input.txt");
var CalValues = ProcessInputStep1(input);
var result = "Step 1: " + CalValues;
PrintOutput(result);
var CalValues2 = ProcessInputStep2(input);
result = "Step 2: " + CalValues2;
PrintOutput(result);

public class Collection(List<Map> hands, List<Map2> hands2)
{
    public List<Map> Hands { get; set; } = hands;
    public List<Map2> Hands2 { get; set; } = hands2;
}

public class Map(string hand, long bid, List<char> cards) : IComparable<Map>
{
    public List<char> Cards { get; set; } = cards;
    public string Hand { get; set; } = hand;
    public long Bid { get; set; } = bid;
    public int CompareTo(Map other)
    {
        if(HandType < other.HandType) return 1;
        else if (HandType > other.HandType) return -1;

        for (int i = 0; i < Hand.Length; i++)
        {
            if (Cards.IndexOf(Hand[i]) < Cards.IndexOf(other.Hand[i])) return 1;
            else if (Cards.IndexOf(Hand[i]) > Cards.IndexOf(other.Hand[i])) return -1;
        }
        return 0;
    }

    public Hands HandType => ResultingHand();

    private Hands ResultingHand()
    {
        var group = Hand.GroupBy(x => x);

        if (Hand.All(x => x.Equals(Hand[0]))) return Hands.FiveoK;
        else if (group.Any(q => q.Count() == 4)) return Hands.FouroK;
        else if ((group.Any(q => q.Count() == 3) && group.Any(q => q.Count() == 2))) return Hands.FullHouse;
        else if (group.Any(q => q.Count() == 3)) return Hands.ThreeoK;
        else if (group.Where(q => q.Count() == 2).Count() == 2) return Hands.TwoPair;
        else if (group.Any(q => q.Count() == 2)) return Hands.TwooK;
        else return Hands.HighCard;    
    }
}
public class Map2(string hand, long bid, List<char> cards) : IComparable<Map2>
{
    public List<char> Cards { get; set; } = cards;
    public string Hand { get; set; } = hand;
    public long Bid { get; set; } = bid;
    public int CompareTo(Map2 other)
    {
        if (HandType < other.HandType) return 1;
        else if (HandType > other.HandType) return -1;

        for (int i = 0; i < Hand.Length; i++)
        {
            if (Cards.IndexOf(Hand[i]) < Cards.IndexOf(other.Hand[i])) return 1;
            else if (Cards.IndexOf(Hand[i]) > Cards.IndexOf(other.Hand[i])) return -1;
        }
        return 0;
    }

    public Hands HandType => ResultingHand();

    private Hands ResultingHand()
    {
        var group = Hand.GroupBy(x => x);

        if (Hand.All(x => x.Equals(Hand[0])) || (Hand.Contains('J') && Hand.Distinct().Count() == 2)) return Hands.FiveoK;
        else if (group.Any(q => q.Count() == 4 ||
            (q.Key != 'J' && q.Count() == 3 && Hand.Contains('J')) ||
            (q.Key != 'J' && q.Count() == 2 && Hand.Count(x => x == 'J') == 2) ||
            (q.Key != 'J' && q.Count() == 1 && Hand.Count(x => x == 'J') == 3))) return Hands.FouroK;
        else if ((group.Any(q => q.Count() == 3) && group.Any(q => q.Count() == 2)) ||
            (group.Where(q => q.Count() == 2 && q.Key != 'J').Count() == 2 && Hand.Contains('J'))) return Hands.FullHouse;
        else if (group.Any(q => q.Count() == 3 ||
            (q.Key != 'J' && q.Count() == 2 && Hand.Contains('J')) ||
            (q.Key != 'J' && q.Count() == 1 && Hand.Count(x => x == 'J') == 2))) return Hands.ThreeoK;
        else if (group.Where(q => q.Count() == 2).Count() == 2) return Hands.TwoPair;
        else if (group.Any(q => q.Count() == 2) || Hand.Contains('J')) return Hands.TwooK;
        else return Hands.HighCard;
    }
}
public enum Hands
{
    FiveoK,
    FouroK,
    FullHouse,
    ThreeoK,
    TwoPair,
    TwooK,
    HighCard
}

public static class LockMe
{
    public static readonly object _sync = new object();
}