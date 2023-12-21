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
    var numCollection = new List<NumberList>();
    foreach(var line in lines)
    {
        var splitLine = line.Split(':');
        var secondSplit = splitLine[1].Split('|');
        var game = int.Parse(splitLine[0].Replace("Card ", "", StringComparison.InvariantCultureIgnoreCase).Trim());
        var winninglist = secondSplit[0].Trim().Split(' ').Where(q => !string.IsNullOrWhiteSpace(q)).Select(q => int.Parse(q)).ToList();
        var selectedList = secondSplit[1].Trim().Split(' ').Where(q => !string.IsNullOrWhiteSpace(q)).Select(q => int.Parse(q)).ToList();

        numCollection.Add(new NumberList(game, winninglist, selectedList));
    }

    return new Collection(numCollection);
};

var ProcessInputStep1 = (List<string> lines) =>
{
    var collection = GenerateCollections(lines);

    var matchingList = collection.NumberCollection.Select(q => {
            var matching = q.SelectedNumbers.Where(s => q.WinningNumbers.Contains(s));
            return Convert.ToInt32(Math.Pow(2, matching.Count() - 1));
        }).ToList();

    return matchingList;
};


var ProcessInputStep2 = (List<string> lines) =>
{
    var winnings = 0;
    var collection = GenerateCollections(lines);
    int iter = 0;

    while (iter < collection.NumberCollection.Count)
    {
        var game = collection.NumberCollection[iter];
        var matching = game.SelectedNumbers.Where(q => game.WinningNumbers.Contains(q)).ToList();
        
        for(var i = 1; i <= matching.Count(); i++)
        {
            var newGame = collection.NumberCollection.FirstOrDefault(q => q.GameNumber == game.GameNumber + i);
            if (newGame == null) break;
            collection.NumberCollection.Add(newGame);
        }

        iter++;
    }
    winnings = collection.NumberCollection.Count();

    return winnings;
};

var PrintOutput = (string result) => Console.WriteLine(result);
var input = GetInput("input.txt");
var CalValues = ProcessInputStep1(input);
var result = "Step 1: " + CalValues.Sum();
PrintOutput(result);
var CalValues2 = ProcessInputStep2(input);
result = "Step 2: " + CalValues2;
PrintOutput(result);


public class Collection(List<NumberList> numberList)
{
    public List<NumberList> NumberCollection { get; set; } = numberList;
}

public class NumberList(int gameNumber, List<int> winningNumbers, List<int> selectedNumbers)
{
    public int GameNumber { get; set; } = gameNumber;
    public List<int> WinningNumbers { get; set; } = winningNumbers;
    public List<int> SelectedNumbers { get; set; } = selectedNumbers;
}