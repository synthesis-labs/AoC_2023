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

var GenerateCollections = (List<string> lines) =>
{
    var lineArray = lines.ToArray();
    var numCollection = new List<NumberList>();
    var specialCollection = new List<SpecialList>();
    for (int x = 0; x < lineArray.Length; x++)
    {
        var numList = "";
        var indexes = new List<int>();
        for (int y = 0; y < lineArray[x].Length; y++)
        {
            if (char.IsDigit(lineArray[x][y]))
            {
                indexes.Add(y);
                numList += lineArray[x][y];
            }
            else if (indexes.Count > 0)
            {
                numCollection.Add(new NumberList(x, indexes, int.Parse(numList)));
                indexes = new List<int>();
                numList = "";
            }

            if (lineArray[x][y] != '.' && !char.IsLetterOrDigit(lineArray[x][y]))
            {
                specialCollection.Add(new SpecialList(x, y, lineArray[x][y]));
            }
        }

        if (indexes.Count > 0)
        {
            numCollection.Add(new NumberList(x, indexes, int.Parse(numList)));
        }
    }

    return new Collection(numCollection, specialCollection);
};

var ProcessInputStep1 = (List<string> lines) =>
{
    var partnum = new List<int>();
    var collection = GenerateCollections(lines);
    
    foreach(var special in collection.SpecialCollection)
    {
        var prevNumbers = collection.NumberCollection.Where(q => q.Line == (special.Line - 1) && 
                (q.Indexes.Contains(special.Index) || 
                 q.Indexes.Contains(special.Index - 1) || 
                 q.Indexes.Contains(special.Index + 1))).ToList();
        var nextNumbers = collection.NumberCollection.Where(q => q.Line == (special.Line + 1) &&
                (q.Indexes.Contains(special.Index) ||
                 q.Indexes.Contains(special.Index - 1) ||
                 q.Indexes.Contains(special.Index + 1))).ToList();
        var curNumbers = collection.NumberCollection.Where(q => q.Line == special.Line &&
                (q.Indexes.Contains(special.Index - 1) ||
                 q.Indexes.Contains(special.Index + 1))).ToList();

        partnum.AddRange(prevNumbers.Select(q => q.Numbers).ToList());
        partnum.AddRange(nextNumbers.Select(q => q.Numbers).ToList());
        partnum.AddRange(curNumbers.Select(q => q.Numbers).ToList());

        collection.NumberCollection = collection.NumberCollection.Where(q => !prevNumbers.Contains(q)).ToList();
        collection.NumberCollection = collection.NumberCollection.Where(q => !nextNumbers.Contains(q)).ToList();
        collection.NumberCollection = collection.NumberCollection.Where(q => !curNumbers.Contains(q)).ToList();
    }

    return partnum;
};


var ProcessInputStep2 = (List<string> lines) =>
{
    var partnum = new List<int>();
    var collection = GenerateCollections(lines);

    foreach (var special in collection.SpecialCollection)
    {
        var prevNumbers = collection.NumberCollection.Where(q => q.Line == (special.Line - 1) &&
                (q.Indexes.Contains(special.Index) ||
                 q.Indexes.Contains(special.Index - 1) ||
                 q.Indexes.Contains(special.Index + 1))).ToList();
        var nextNumbers = collection.NumberCollection.Where(q => q.Line == (special.Line + 1) &&
                (q.Indexes.Contains(special.Index) ||
                 q.Indexes.Contains(special.Index - 1) ||
                 q.Indexes.Contains(special.Index + 1))).ToList();
        var curNumbers = collection.NumberCollection.Where(q => q.Line == special.Line &&
                (q.Indexes.Contains(special.Index - 1) ||
                 q.Indexes.Contains(special.Index + 1))).ToList();

        var calc = 0;
        var prevCount = prevNumbers.Count();
        var nextCount = nextNumbers.Count();
        var curCount = curNumbers.Count();

        if(prevCount == 2 && nextCount == 0 && curCount == 0)
        {
            calc = prevNumbers[0].Numbers * prevNumbers[1].Numbers;
        }

        if (prevCount == 0 && nextCount == 2 && curCount == 0)
        {
            calc = nextNumbers[0].Numbers * nextNumbers[1].Numbers;
        }

        if (prevCount == 0 && nextCount == 0 && curCount == 2)
        {
            calc = curNumbers[0].Numbers * curNumbers[1].Numbers;
        }

        if (prevCount == 1 && nextCount == 1 && curCount == 0)
        {
            calc = prevNumbers.First().Numbers * nextNumbers.First().Numbers;
        }

        if (prevCount == 1 && nextCount == 0 && curCount == 1)
        {
            calc = prevNumbers.First().Numbers * curNumbers.First().Numbers;
        }

        if (prevCount == 0 && nextCount == 1 && curCount == 1)
        {
            calc = nextNumbers.First().Numbers * curNumbers.First().Numbers;
        }

        partnum.Add(calc);

    }

    return partnum;
};

var PrintOutput = (string result) => Console.WriteLine(result);
var input = GetInput("input.txt");
var CalValues = ProcessInputStep1(input);
var result = "Step 1: " + CalValues.Sum();
PrintOutput(result);
var CalValues2 = ProcessInputStep2(input);
result = "Step 2: " + CalValues2.Sum();
PrintOutput(result);


public class Collection(List<NumberList> numberList, List<SpecialList> specialList) 
{
    public List<NumberList> NumberCollection { get; set; } = numberList;
    public List<SpecialList> SpecialCollection { get; set; } = specialList;
}

public class NumberList(int Line, List<int> Indexes, int Numbers)
{
    public int Line { get; set; } = Line;
    public List<int> Indexes { get; set; } = Indexes;
    public int Numbers { get; set; } = Numbers;
}

public class SpecialList(int Line, int Index, char Special)
{
    public int Line { get; set; } = Line;
    public int Index { get; set; } = Index;
    public char Special { get; set; } = Special;
}