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

        if (current.Any(q => q[2] == 'Z'))
        {
            Console.SetCursorPosition(0, 3);
            Console.WriteLine(string.Join(',', current));
        }
    }
};

var GenerateCollections = (List<string> lines) =>
{
    var res = new List<Map>();
    var steps = lines[0].Select(ch => ch).ToList();

    foreach (var line in lines.Skip(2))
    {
        var temp = line.Split('=');
        var node = temp[0].Trim();
        var nodelist = temp[1].Trim().Split(',');
        var leftnode = nodelist[0].Trim('(').Trim();
        var rightnode = nodelist[1].Trim(')').Trim();
        res.Add(new Map(node, leftnode, rightnode));
    }

    return new Collection(res, steps);
};

var ProcessInputStep1 = (List<string> lines) =>
{
    var collection = GenerateCollections(lines);
    var found = false;
    var current = "AAA";
    var stepNo = 0;

    long res = 0;
    while (!found)
    {
        res++;

        if (collection.Steps.Count() == stepNo) stepNo = 0;

        var step = collection.Steps[stepNo];
        stepNo++;

        if (step == 'L')
        {
            current = collection.Nodes.FirstOrDefault(q => string.Equals(q.Node, current, StringComparison.InvariantCultureIgnoreCase))?.LeftNode;
        }

        if (step == 'R')
        {
            current = collection.Nodes.FirstOrDefault(q => string.Equals(q.Node, current, StringComparison.InvariantCultureIgnoreCase))?.RightNode;
        }

        if (current == "ZZZ") found = true;
    }

    return res;
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
    var found = false;
    var current = new List<string>();
    var stepNo = 0;

    long res = 0;
    var steps = new List<long>();

    collection.Nodes.Where(q => q.Node[2] == 'A').Select(s => s.Node).ToList().ForEach(q => current.Add(q));

    for (var i = 0; i < current.Count(); i++)
    {
        res = 0;
        found = false;
        var curr = current[i];
        while (!found)
        {
            res++;

            if (collection.Steps.Count() == stepNo) stepNo = 0;

            var step = collection.Steps[stepNo];
            stepNo++;

            if (step == 'L')
            {
                curr = collection.Nodes.FirstOrDefault(q => string.Equals(q.Node, curr, StringComparison.InvariantCultureIgnoreCase))?.LeftNode;
            }

            if (step == 'R')
            {
                curr = collection.Nodes.FirstOrDefault(q => string.Equals(q.Node, curr, StringComparison.InvariantCultureIgnoreCase))?.RightNode;
            }

            if (curr[2] == 'Z') found = true;
        }
        PrintOutput(res.ToString());
        steps.Add(res);
    }
    var least = LCM(steps);

    return least;
};

var input = GetInput("input.txt");
var CalValues = ProcessInputStep1(input);
var result = "Step 1: " + CalValues;
PrintOutput(result);
var CalValues2 = ProcessInputStep2(input);
result = "Step 2: " + CalValues2;
PrintOutput(result);

public class Collection(List<Map> nodes, List<char> steps)
{
    public List<Map> Nodes { get; set; } = nodes;
    public List<char> Steps { get; set; } = steps;
}

public class Map(string node, string left, string right)
{
    public string Node { get; set; } = node;
    public string LeftNode { get; set; } = left;
    public string RightNode { get; set; } = right;
}

public static class LockMe
{
    public static readonly object _sync = new object();
}