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

var ProcessInputStep1 = (string line) =>
{
    var convertedList = line.ToCharArray().Where(x => char.IsDigit(x)).Select(y => y - '0').ToList();
    return int.Parse($"{convertedList.First()}{convertedList.Last()}");
};

Dictionary<string, string> numbers = new Dictionary<string, string>(){{"zero","ze0o"},{"one","o1e"},{"two","t2o"},{"three","th3ee"},{"four","fo4r"},{"five","fi5ve"},{"six","s6x"},{"seven","se7en"},{"eight","ei8ht"},{"nine","ni9ne"}};

var ProcessInputStep2 = (string line) =>
{
    var foundNumberListIndex = new List<KeyValuePair<int, string>>();
    foreach (var number in numbers) line = line.Replace(number.Key, number.Value);
    var converted = line.ToCharArray().Where(x => char.IsDigit(x)).Select(y => y - '0').ToList();
    return int.Parse($"{converted.First()}{converted.Last()}");
};

var PrintOutput = (string result) => Console.WriteLine(result);
var input = GetInput("input.txt");
var CalValues = input.Select(x => ProcessInputStep1(x));
var result = "Step 1:" + CalValues.Sum().ToString();
PrintOutput(result);
var CalValues2 = input.Select(x => ProcessInputStep2(x));
result = "Step 2:" + CalValues2.Sum().ToString();
PrintOutput(result);
