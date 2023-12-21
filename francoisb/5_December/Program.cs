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
    lock(LockMe._sync)
    {
        Console.SetCursorPosition(0, Convert.ToInt32(pos + 2));
        var percentage = (Convert.ToDecimal(Count)-Convert.ToDecimal(Start)) / (Convert.ToDecimal(Remaining)-Convert.ToDecimal(Start)) * 100;
        Console.WriteLine($"{Thread}: {Count} / {Remaining} {Math.Round(percentage, 2)} %");        
    }
};

var GenerateCollections = (List<string> lines, bool step2) =>
{
    var seedCollection = new List<SeedAllocation>();
    var seedRangeCollection = new List<SeedRangeAllocation>();
    var SeedToSoil = new List<Map>();
    var SoilToFertilizer = new List<Map>();
    var FertilizerToWater = new List<Map>();
    var WaterToLight = new List<Map>();
    var LightToTemperature = new List<Map>();
    var TemperatureToHumidity = new List<Map>();
    var HumidityToLocation = new List<Map>();
    var sectionName = "";
    foreach (var line in lines)
    {
        if (line.Contains(':')) // new Section
        {
            var splitLine = line.Split(':');
            if (string.Equals(splitLine[0], "seeds", StringComparison.InvariantCultureIgnoreCase))
            {
                var numLine = splitLine[1].Trim().Split(' ');
                if (step2)
                {
                    for (var x = 0; x < numLine.Count(); x += 2)
                    {
                        var seedStart = long.Parse(numLine[x]);
                        var seedRange = long.Parse(numLine[x + 1]) - 1;

                        var seed = new SeedRangeAllocation(x+1, seedStart, seedStart + seedRange, seedRange);
                        seedRangeCollection.Add(seed);
                    }
                }
                else
                {
                    foreach (var item in numLine)
                    {
                        var seedNum = long.Parse(item);
                        var seed = new SeedAllocation(seedNum);
                        seedCollection.Add(seed);
                    }
                }
            }

            sectionName = splitLine[0];
        }
        else if (!string.IsNullOrEmpty(line))
        {

            var section = line.Split(' ');
            var dest = long.Parse(section[0]);
            var source = long.Parse(section[1]);
            var range = long.Parse(section[2]);
            var destEnd = dest + range - 1;
            var sourceEnd = source + range - 1;
            switch (sectionName)
            {
                case "seed-to-soil map":
                    SeedToSoil.Add(new Map(dest, destEnd, source, sourceEnd, range));
                    break;
                case "soil-to-fertilizer map":
                    SoilToFertilizer.Add(new Map(dest, destEnd, source, sourceEnd, range));
                    break;
                case "fertilizer-to-water map":
                    FertilizerToWater.Add(new Map(dest, destEnd, source, sourceEnd, range));
                    break;
                case "water-to-light map":
                    WaterToLight.Add(new Map(dest, destEnd, source, sourceEnd, range));
                    break;
                case "light-to-temperature map":
                    LightToTemperature.Add(new Map(dest, destEnd, source, sourceEnd, range));
                    break;
                case "temperature-to-humidity map":
                    TemperatureToHumidity.Add(new Map(dest, destEnd, source, sourceEnd, range));
                    break;
                case "humidity-to-location map":
                    HumidityToLocation.Add(new Map(dest, destEnd, source, sourceEnd, range));
                    break;
                default:
                    break;
            }
        }
    }

    return new Collection(seedCollection, seedRangeCollection, SeedToSoil, SoilToFertilizer, FertilizerToWater, WaterToLight, LightToTemperature, TemperatureToHumidity, HumidityToLocation);
};

var MapSrcToDest = (List<Map> Maps, List<long> location) =>
{
    var last = location.Last();
    var matching = Maps.Where(q => q.SourceStart <= last && q.SourceEnd >= last).FirstOrDefault();
    if (matching != null)
    {
        long diff = last - matching.SourceStart;
        location.Add(matching.DestinationStart + diff);
    }
    else
    {
        location.Add(last);
    }
};

var ProcessInputStep1 = (List<string> lines, bool step2) =>
{
    var collection = GenerateCollections(lines, step2);

    var locations = new ConcurrentBag<long>();

    Parallel.ForEach(collection.SeedCollection, new ParallelOptions() { MaxDegreeOfParallelism = 16 }, q =>
    {
        var minLocation = long.MaxValue;
        var locs = new List<long> { q.Seed };

        MapSrcToDest(collection.SeedToSoil, locs);
        MapSrcToDest(collection.SoilToFertilizer, locs);
        MapSrcToDest(collection.FertilizerToWater, locs);
        MapSrcToDest(collection.WaterToLight, locs);
        MapSrcToDest(collection.LightToTemperature, locs);
        MapSrcToDest(collection.TemperatureToHumidity, locs);
        MapSrcToDest(collection.HumidityToLocation, locs);

        minLocation = Math.Min(locs.Last(), minLocation);

        locations.Add(minLocation);
    });

    return locations.Min();
};

var ProcessInputStep2 = (List<string> lines) =>
{
    var collection = GenerateCollections(lines, true);

    var locations = new ConcurrentBag<long>();

    Parallel.ForEach(collection.SeedRangeCollection, new ParallelOptions() { MaxDegreeOfParallelism = 16 }, (q, i, x) =>
    {
        var minLocation = long.MaxValue;
        for (long y = q.Seed; y < q.SeedEnd; y++)
        {
            if (y % 1000 == 0 || y == q.SeedEnd)
            {
                PrintProgress($"Thread {x}", y, q.Seed, q.SeedEnd, x);
            }
            var locations = new List<long>() { y };

            MapSrcToDest(collection.SeedToSoil, locations);
            MapSrcToDest(collection.SoilToFertilizer, locations);
            MapSrcToDest(collection.FertilizerToWater, locations);
            MapSrcToDest(collection.WaterToLight, locations);
            MapSrcToDest(collection.LightToTemperature, locations);
            MapSrcToDest(collection.TemperatureToHumidity, locations);
            MapSrcToDest(collection.HumidityToLocation, locations);

            minLocation = Math.Min(locations.Last(), minLocation);
        }
            PrintProgress($"Thread {x}", q.SeedEnd, q.Seed, q.SeedEnd, x);

        locations.Add(minLocation);
    });


    return locations.Min();
};

var input = GetInput("input.txt");
var CalValues = ProcessInputStep1(input, false);
var result = "Step 1: " + CalValues;
PrintOutput(result);
var CalValues2 = ProcessInputStep2(input);
result = "Step 2: " + CalValues2;
PrintOutput(result);

public class SeedAllocation(long seedStart)
{
    public long Seed { get; set; } = seedStart;
    public long Soil { get; set; }
    public long Fertilizer { get; set; }
    public long Water { get; set; }
    public long Light { get; set; }
    public long Temperature { get; set; }
    public long Humidity { get; set; }
    public long Location { get; set; }
}

public class SeedRangeAllocation(int iteration, long seedStart, long seedEnd, long seedRange)
{
    public int Iteration { get; set; } = iteration;
    public long Seed { get; set; } = seedStart;
    public long SeedEnd { get; set; } = seedEnd;
    public long SeedRange { get; set; } = seedRange;
    public long Location { get; set; }
}


public class Collection(List<SeedAllocation> seeds, List<SeedRangeAllocation> seedRange, List<Map> sts, List<Map> stf, List<Map> ftw, List<Map> wtl, List<Map> ltt, List<Map> tth, List<Map> htl)
{
    public List<SeedAllocation> SeedCollection { get; set; } = seeds;
    public List<SeedRangeAllocation> SeedRangeCollection { get; set; } = seedRange;
    public List<Map> SeedToSoil { get; set; } = sts;
    public List<Map> SoilToFertilizer { get; set; } = stf;
    public List<Map> FertilizerToWater { get; set; } = ftw;
    public List<Map> WaterToLight { get; set; } = wtl;
    public List<Map> LightToTemperature { get; set; } = ltt;
    public List<Map> TemperatureToHumidity { get; set; } = tth;
    public List<Map> HumidityToLocation { get; set; } = htl;
}

public class Map(long destStart, long destEnd, long sourceStart, long sourceEnd, long range)
{
    public long DestinationStart { get; set; } = destStart;
    public long DestinationEnd { get; set; } = destEnd;
    public long SourceStart { get; set; } = sourceStart;
    public long SourceEnd { get; set; } = sourceEnd;
    public long Range { get; set; } = range;
}

public static class LockMe 
{
    public static readonly object _sync = new object();
}