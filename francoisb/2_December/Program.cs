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

var GetNumber = (string input, string color, string defaultvalue = "0") => int.Parse((input.Split(',').FirstOrDefault(q => q.Contains(color, StringComparison.InvariantCultureIgnoreCase)) ?? defaultvalue).Replace(" ", "").Replace(color, "", StringComparison.InvariantCultureIgnoreCase));

var ProcessInputStep1 = (List<string> lines, int r, int g, int b) =>
{
    var possibleGameSet = 0;
    foreach (var game in lines)
    {
        var split = game.Split(":");
        var gameNumber = int.Parse(split[0].Replace(" ", "").Replace("Game", ""));
        var revealedSets = split[1].Split(";");
        var possibleset = true;
        foreach (var item in revealedSets)
        {
            var red = GetNumber(item, "red", "0");
            var green = GetNumber(item, "green", "0");
            var blue = GetNumber(item, "blue", "0");
            if (red > r || green > g || blue > b) possibleset = false;
        }
        possibleGameSet += possibleset ? gameNumber : 0;
    }
    return possibleGameSet;
};


var ProcessInputStep2 = (List<string> lines) =>
{
    List<int> powergame = new List<int>();
    foreach (var game in lines)
    {
        var split = game.Split(":");
        var revealedSets = split[1].Split(";");
        var setTotal = new { red = new List<int>(), green = new List<int>(), blue = new List<int>() };
        foreach (var item in revealedSets)
        {
            setTotal.red.Add(GetNumber(item, "red", "1"));
            setTotal.green.Add(GetNumber(item, "green", "1"));
            setTotal.blue.Add(GetNumber(item, "blue", "1"));
        }
        powergame.Add(setTotal.red.Max() * setTotal.green.Max() * setTotal.blue.Max());
    }
    return powergame;
};

var PrintOutput = (string result) => Console.WriteLine(result);
var input = GetInput("input.txt");
var CalValues = ProcessInputStep1(input, 12, 13, 14);
var result = "Step 1:" + CalValues;
PrintOutput(result);
var CalValues2 = ProcessInputStep2(input);
result = "Step 2:" + CalValues2.Sum().ToString();
PrintOutput(result);
