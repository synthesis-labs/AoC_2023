var GetInput = (string inputFileName) =>
{
    List<string> result = new List<string>();
    string line;
    StreamReader sr = new StreamReader(inputFileName);
    line = sr.ReadLine();
    while (line != null)
    {
        result.Add(line);
        line = sr.ReadLine();
    }
    sr.Close();
    return result;
};
var PrintOutput = (string result) => Console.WriteLine(result);
var input = GetInput("input.txt");
input.ForEach(x => PrintOutput(x));
