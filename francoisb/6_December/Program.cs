using System.Collections.Concurrent;

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
    var timeLine = lines[0].Split(':')[1];
    var distLine = lines[1].Split(':')[1];
    var times = timeLine.Trim().Split(' ').Where(q => !string.IsNullOrWhiteSpace(q)).ToList();
    var distances = distLine.Trim().Split(' ').Where(q => !string.IsNullOrWhiteSpace(q)).ToList();

    var races = new List<Map>();

    for(int i = 0; i < times.Count(); i++)
    {
        var map = new Map(int.Parse(times[i]), int.Parse(distances[i]));

        races.Add(map);
    }
    
    
    return new Collection(races);
};

var ProcessInputStep1 = (List<string> lines) =>
{
    var collection = GenerateCollections(lines);
    var result = new ConcurrentBag<long>();

    Parallel.ForEach(collection.Races, (race, i, thread) =>
    {
        var wins = 0;
        for(int x = 0; x < race.Time; x++)
        {
            if (x % 1 == 0 || x == race.Time)
            {
                PrintProgress($"Thread {thread}", x, 0, race.Time, thread);
            }
            var dist = x * (race.Time - x);
            if(dist > race.Distance)
            {
                wins++;
            }
        }
        PrintProgress($"Thread {thread}", race.Time, 0, race.Time, thread);

        result.Add(wins);
    });

    return result.Aggregate((x, y) => x * y);
};

var ProcessInputStep2 = (List<string> lines) =>
{
    var collection = GenerateCollections(lines);
    var map = new Map(Convert.ToInt64(string.Join("", collection.Races.Select(q => q.Time.ToString()))), Convert.ToInt64(string.Join("", collection.Races.Select(q => q.Distance.ToString()))));
    
    var wins = 0;
    for (int x = 0; x < map.Time; x++)
    {
        var dist = x * (map.Time - x);
        if (dist > map.Distance)
        {
            wins++;
        }
    }

    return wins;
};

var input = GetInput("input.txt");
var CalValues = ProcessInputStep1(input);
var result = "Step 1: " + CalValues;
PrintOutput(result);
var CalValues2 = ProcessInputStep2(input);
result = "Step 2: " + CalValues2;
PrintOutput(result);

public class Collection(List<Map> races)
{
    public List<Map> Races { get; set; } = races;
}

public class Map(long time, long distance)
{
    public long Time { get; set; } = time;
    public long Distance { get; set; } = distance;
}

public static class LockMe
{
    public static readonly object _sync = new object();
}