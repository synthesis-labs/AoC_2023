using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2023
{
    internal class Day5
    {
        String[] lines = File.ReadAllLines("input.day5.txt");
        List<double> seeds = new List<double>();
        Dictionary<Section, List<GardenMap>> maps = new Dictionary<Section, List<GardenMap>>();
        List<double> locationIds = new List<double>();
        object locker = new object();
        double lowestLocationId;

        internal void ExecutePart1()
        {
            ParseInput();
            FindClosestLocationsForSeeds();
            Console.WriteLine("Day 5, Part 1: " + locationIds.Min());
        }

        internal void ExecutePart2ThatTakes30DamnMinutes()
        {
            FindClosestLocationsForSeedsRanges();
            Console.WriteLine("Day 5, Part 2: " + lowestLocationId);
        }

        private void ParseInput()
        {
            var section = Section.seeds;
            foreach (var line in lines)
            {
                if (line == "")
                    continue;

                if (line.StartsWith("seeds"))
                {
                    section = Section.seeds;
                    var seedStrings = line.Split(':')[1].Split(' ');
                    seeds.AddRange(seedStrings.Where(x => x != "").Select(x => Double.Parse(x)));
                    continue;
                }
                else if (line.StartsWith("seed-to-soil"))
                {
                    section = Section.seed_to_soil;
                    continue;
                }
                else if (line.StartsWith("soil-to-fertilizer"))
                {
                    section = Section.soil_to_fertilizer;
                    continue;
                }
                else if (line.StartsWith("fertilizer-to-water"))
                {
                    section = Section.fertilizer_to_water;
                    continue;
                }
                else if (line.StartsWith("water-to-light"))
                {
                    section = Section.water_to_light;
                    continue;
                }
                else if (line.StartsWith("light-to-temperature"))
                {
                    section = Section.light_to_temperature;
                    continue;
                }
                else if (line.StartsWith("temperature-to-humidity"))
                {
                    section = Section.temperature_to_humidity;
                    continue;
                }
                else if (line.StartsWith("humidity-to-location"))
                {
                    section = Section.humidity_to_location;
                    continue;
                }

                var values = line.Split(' ');

                if (!maps.ContainsKey(section))
                    maps.Add(section, new List<GardenMap>());

                maps[section].Add(new GardenMap()
                {
                    destinationRangeStart = Double.Parse(values[0]),
                    sourceRangeStart = Double.Parse(values[1]),
                    rangeLength = Double.Parse(values[2])
                });
            }

            for (int i = 0; i < maps.Count; i++)
            {
                var map = maps.ElementAt(i);
                maps[map.Key] = maps[map.Key].OrderBy(x => x.sourceRangeStart).ToList();

                if (maps[map.Key].ElementAt(0).sourceRangeStart != 0)
                {
                    maps[map.Key].Insert(0, new GardenMap()
                    {
                        sourceRangeStart = 0,
                        destinationRangeStart = 0,
                        rangeLength = maps[map.Key].ElementAt(0).sourceRangeStart - 1
                    });
                }
            }
        }

        private void FindClosestLocationsForSeeds()
        {
            foreach (var seed in seeds)
                locationIds.Add(GetLocationId(seed));
        }

        private void FindClosestLocationsForSeedsRanges()
        {
            List<SeedRange> ranges = new List<SeedRange>();
            for (int i = 0; i < seeds.Count; i++)
            {
                var seed = seeds[i];
                var range = seeds[i + 1];
                ranges.Add(new SeedRange { seed = seed, range = range });
                i++;
            }
            ParallelOptions options = new ParallelOptions()
            {
                MaxDegreeOfParallelism = 10
            };
            lowestLocationId = maps[Section.humidity_to_location].Max(x => x.destinationRangeStart);
            Parallel.ForEach(ranges, options, (seed) =>
            {
                for (int i = 0; i < seed.range; i++)
                {
                    var locationId = GetLocationId(seed.seed + i);
                    if (locationId < lowestLocationId)
                    {
                        lock (locker)
                            lowestLocationId = locationId;
                    }
                }
            });
        }

        private double GetLocationId(double seed)
        {
            var soilId = GetMap(Section.seed_to_soil, seed);
            var fertilizerId = GetMap(Section.soil_to_fertilizer, soilId);
            var waterId = GetMap(Section.fertilizer_to_water, fertilizerId);
            var lightId = GetMap(Section.water_to_light, waterId);
            var temperatureId = GetMap(Section.light_to_temperature, lightId);
            var humidityId = GetMap(Section.temperature_to_humidity, temperatureId);
            var locationId = GetMap(Section.humidity_to_location, humidityId);

            return locationId;
        }

        private double GetMap(Section section, double source)
        {
            var map = maps[section].FirstOrDefault(x => x.sourceRangeStart <= source && source <= (x.sourceRangeStart + (x.rangeLength - 1)));

            var sourceRangeStart = map == null ? source : map.sourceRangeStart;
            var desinationRangeStart = map == null ? source : map.destinationRangeStart;
            double offset = map == null ? 0 : source - map.sourceRangeStart;

            return desinationRangeStart + offset;
        }

        private IEnumerable<double> GenerateDoubleRange(double start, double end, double step)
        {
            return Enumerable.Range(0, (int)((end - start) / step) + 1)
                             .Select(i => start + i * step);
        }
    }

    internal enum Section
    {
        seeds,
        seed_to_soil,
        soil_to_fertilizer,
        fertilizer_to_water,
        water_to_light,
        light_to_temperature,
        temperature_to_humidity,
        humidity_to_location
    }

    internal class GardenMap
    {
        internal double destinationRangeStart { get; set; }
        internal double sourceRangeStart { get; set; }
        internal double rangeLength { get; set; }
    }

    internal class SeedRange
    {
        internal double seed { get; set; }
        internal double range { get; set; }
    }
}


/*
 seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
 */