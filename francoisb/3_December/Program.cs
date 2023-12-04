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
        var numberList = collection.NumberCollection.Where(q => 
                (q.Line == (special.Line - 1) || 
                 q.Line == (special.Line + 1) || 
                 q.Line == special.Line) && 
                (q.Indexes.Contains(special.Index) || 
                 q.Indexes.Contains(special.Index - 1) || 
                 q.Indexes.Contains(special.Index + 1))).ToList();

        partnum.AddRange(numberList.Select(q => q.Numbers).ToList());

        collection.NumberCollection = collection.NumberCollection.Where(q => !numberList.Contains(q)).ToList();
    }

    return partnum;
};


var ProcessInputStep2 = (List<string> lines) =>
{
    var partnum = new List<int>();
    var collection = GenerateCollections(lines);

    foreach (var special in collection.SpecialCollection)
    {
        var numberList = collection.NumberCollection.Where(q =>
                (q.Line == (special.Line - 1) ||
                 q.Line == (special.Line + 1) ||
                 q.Line == special.Line) &&
                (q.Indexes.Contains(special.Index) ||
                 q.Indexes.Contains(special.Index - 1) ||
                 q.Indexes.Contains(special.Index + 1))).ToList();

        var calc = 0;

        if(numberList.Count() == 2)
        {
            calc = numberList[0].Numbers * numberList[1].Numbers;
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