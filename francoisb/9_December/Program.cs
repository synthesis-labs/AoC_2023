using System.Collections.Concurrent;
using System.Reflection.Metadata.Ecma335;
using System.Runtime.ConstrainedExecution;
using System.Xml.Linq;

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

var PrintProgress = (int step, long res, List<string> current) =>
{
    lock (LockMe._sync)
    {
        Console.SetCursorPosition(0, 0);
        Console.WriteLine($"{string.Join(',', current)}: {step} / {res}");
    }
};

var GenerateCollections = (List<string> lines) =>
{
    var res = new List<Map>();
    var steps = lines[0].Select(ch => ch).ToList();

    foreach (var line in lines)
    {
        var temp = line.Split(' ').Select(x => Convert.ToInt64(x)).ToList();
        res.Add(new Map(new List<List<long>>() { temp }));
    }

    return new Collection(res);
};

var ProcessInputStep1 = (List<string> lines) =>
{
    var collection = GenerateCollections(lines);
    ConcurrentBag<long> totals = new ConcurrentBag<long>();
    Parallel.ForEach(collection.Maps, (map, i, thread) =>
    {
        var complete = false;
        var x = 0;
        while (!complete)
        {
            long prev = 0;
            long div = 0;
            List<long> nextnode = new List<long>();
            for (var y = 1; y < map.Nodes[x].Count(); y++)
            {
                div = map.Nodes[x][y] - map.Nodes[x][y - 1];
                nextnode.Add(div);
            }

            map.Nodes.Add(nextnode);
            if(nextnode.All(x => x == 0)) complete = true;
            x++;
        }

        for(var y = map.Nodes.Count()-1;  y > 0; y--)
        {
            var cur = map.Nodes[y].Last();
            if (cur == 0) continue;
            var prev = map.Nodes[y - 1].Last();
            map.Nodes[y-1].Add(cur + prev);
        }
        PrintOutput(map.Nodes.First().Last().ToString());

         totals.Add(map.Nodes.First().Last());
    });

    return totals.Sum();
};

static long GCD(long x, long y)
{
    if (y == 0) return x;
    else return GCD(y, x % y);
};

var LCM = (List<long> steps) => steps.Aggregate((x, y) => x * y / GCD(x, y));

var ProcessInputStep2 = (List<string> lines) =>
{
    var collection = GenerateCollections(lines);
    ConcurrentBag<long> totals = new ConcurrentBag<long>();
    Parallel.ForEach(collection.Maps, (map, i, thread) =>
    {
        var complete = false;
        var x = 0;
        while (!complete)
        {
            long prev = 0;
            long div = 0;
            List<long> nextnode = new List<long>();
            for (var y = 1; y < map.Nodes[x].Count(); y++)
            {
                div = map.Nodes[x][y] - map.Nodes[x][y - 1];
                nextnode.Add(div);
            }

            map.Nodes.Add(nextnode);
            if (nextnode.All(x => x == 0)) complete = true;
            x++;
        }

        for (var y = map.Nodes.Count() - 1; y > 0; y--)
        {
            var cur = map.Nodes[y].First();
            if (cur == 0) continue;
            var prev = map.Nodes[y - 1].First();
            map.Nodes[y-1] = map.Nodes[y - 1].Prepend(prev-cur).ToList();
        }
        PrintOutput(map.Nodes.First().First().ToString());

        totals.Add(map.Nodes.First().First());
    });

    return totals.Sum();
};

var input = GetInput("input.txt");
var CalValues = ProcessInputStep1(input);
var result = "Step 1: " + CalValues;
PrintOutput(result);
var CalValues2 = ProcessInputStep2(input);
result = "Step 2: " + CalValues2;
PrintOutput(result);

public class Collection(List<Map> maps)
{
    public List<Map> Maps { get; set; } = maps;
}

public class Map(List<List<long>> nodes)
{
    public List<List<long>> Nodes { get; set; } = nodes;
}

public static class LockMe
{
    public static readonly object _sync = new object();
}