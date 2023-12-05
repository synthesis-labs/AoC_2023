package io.nanovc.aoc2023.day05q2;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.stream.Collectors;

/**
 * --- Part Two ---
 * Everyone will starve if you only plant such a small number of seeds. Re-reading the almanac, it looks like the seeds: line actually describes ranges of seed numbers.
 *
 * The values on the initial seeds: line come in pairs. Within each pair, the first value is the start of the range and the second value is the length of the range. So, in the first line of the example above:
 *
 * seeds: 79 14 55 13
 * This line describes two ranges of seed numbers to be planted in the garden. The first range starts with seed number 79 and contains 14 values: 79, 80, ..., 91, 92. The second range starts with seed number 55 and contains 13 values: 55, 56, ..., 66, 67.
 *
 * Now, rather than considering four seed numbers, you need to consider a total of 27 seed numbers.
 *
 * In the above example, the lowest location number can be obtained from seed number 82, which corresponds to soil 84, fertilizer 84, water 84, light 77, temperature 45, humidity 46, and location 46. So, the lowest location number is 46.
 *
 * Consider all of the initial seed numbers listed in the ranges on the first line of the almanac. What is the lowest location number that corresponds to any of the initial seed numbers?
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2023/day/5#part2">Challenge</a>
 */
public abstract class Day05Q2Tests extends TestBase
{

    /**
     * The day that we are solving the puzzle for.
     * eg: "Day 01"
     *
     * @return The day that we are solving the puzzle for.
     */
    @Override
    protected String getDayLabel()
    {
        return "Day 05 Q2";
    }

    /**
     * Gets the sample input that was provided by the puzzle question.
     *
     * @return The sample input that was provided by the puzzle question.
     */
    @Override
    protected String getSampleInput()
    {
        return """
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
                """;
    }

    /**
     * Gets the sample answer that was provided by the puzzle question.
     *
     * @return The sample answer that was provided by the puzzle question.
     */
    @Override
    protected String getSampleAnswer()
    {
        return "46";
    }

    /**
     * This tests that the {@link #solve(String) solution}
     * gets the {@link #getSampleAnswer() sample answer}
     * by using the {@link #getSampleInput() sample input}.
     */
    @Test
    @Override
    public void testSample()
    {
        super.testSample();
    }

    /**
     * Gets the actual answer that we compute as the solution.
     * Usually you leave this blank initially, let the test fail and then update it to what the solution produces from {@link #solve(String)}.
     *
     * @return The actual answer that we compute as the solution.
     */
    @Override
    protected String getActualAnswer()
    {
        return "289863851";
    }

    public static class Solution1Tests extends Day05Q2Tests
    {
        public record GroupText(String groupName, String text) {}
        public record MapInfo(long destinationRangeStart, long sourceRangeStart, long rangeLength) {}
        public record GroupSplits(String groupName, List<MapInfo> numbers) {}

        public record Seed(long seedNumber) {}
        public record SeedRange(long seedStart, long length) {}

        public record MappingResult(
                long seedNumber,
                long soilNumber,
                long fertilizerNumber,
                long waterNumber,
                long lightNumber,
                long temperatureNumber,
                long humidityNumber,
                long locationNumber)
        {
        }

        @Override
        public String solve(String input)
        {
            // Strip off the first line for the seeds:
            var seedInfo = Arrays.stream(input.split("\n\n")).limit(1).findFirst().get();

            // Parse the seed ranges:
            List<Long> seedNumbers = Arrays.stream(seedInfo.split(": ")[1].split("\\s+")).map(Long::parseLong).toList();
            List<SeedRange> seedRanges = new ArrayList<>();
            for (int i = 0; i < seedNumbers.size()/2; i++)
            {
                // Create the seed range:
                var seedRange = new SeedRange(seedNumbers.get(i << 1), seedNumbers.get((i << 1) + 1));

                // Save the range:
                seedRanges.add(seedRange);
            }

            // Create a map of the seeds:
            Map<Long, Seed> seedMap = new HashMap<>();

            // Parse the maps:
            var groupedMapInfo = Arrays.stream(input.split("\n\n"))
                    .skip(1)
                    .map(s -> s.split(":\\n"))
                    .map(s -> new GroupText(s[0], s[1]))
                    .map(g -> new GroupSplits(g.groupName(), Arrays.stream(g.text().split("\\n")).map(s-> Arrays.stream(s.split("\\s+")).map(Long::parseLong).toList()).map(i-> new MapInfo(i.get(0), i.get(1), i.get(2))).toList()))
                    .collect(Collectors.groupingBy(GroupSplits::groupName));

            // Get the various bits of info:
            GroupSplits seedToSoilGroup            = groupedMapInfo.get("seed-to-soil map").get(0);
            GroupSplits soilToFertilizerGroup      = groupedMapInfo.get("soil-to-fertilizer map").get(0);
            GroupSplits fertilizerToWaterGroup     = groupedMapInfo.get("fertilizer-to-water map").get(0);
            GroupSplits waterToLightGroup          = groupedMapInfo.get("water-to-light map").get(0);
            GroupSplits lightToTemperatureGroup    = groupedMapInfo.get("light-to-temperature map").get(0);
            GroupSplits temperatureToHumidityGroup = groupedMapInfo.get("temperature-to-humidity map").get(0);
            GroupSplits humidityToLocationGroup    = groupedMapInfo.get("humidity-to-location map").get(0);

            // Index the map info:
            TreeMap<Long, MapInfo> seedToSoilMap            = indexMapInfoBySourceStart(seedToSoilGroup            );
            TreeMap<Long, MapInfo> soilToFertilizerMap      = indexMapInfoBySourceStart(soilToFertilizerGroup      );
            TreeMap<Long, MapInfo> fertilizerToWaterMap     = indexMapInfoBySourceStart(fertilizerToWaterGroup     );
            TreeMap<Long, MapInfo> waterToLightMap          = indexMapInfoBySourceStart(waterToLightGroup          );
            TreeMap<Long, MapInfo> lightToTemperatureMap    = indexMapInfoBySourceStart(lightToTemperatureGroup    );
            TreeMap<Long, MapInfo> temperatureToHumidityMap = indexMapInfoBySourceStart(temperatureToHumidityGroup );
            TreeMap<Long, MapInfo> humidityToLocationMap    = indexMapInfoBySourceStart(humidityToLocationGroup    );


            // Keep track of the minimum locationNumber:
            long minLocationNumber = Long.MAX_VALUE;

            // Process each seed through the mappings:
            for (int r = 0; r < seedRanges.size(); r++)
            {
                // Get the seed range:
                var seedRange = seedRanges.get(r);
                System.out.println("range: = " + r + " " + seedRange);

                // Go through each seed in the range:
                for (long i = 0, seedNumber = seedRange.seedStart(); i < seedRange.length(); i++, seedNumber++)
                {
                    // Map the entries:
                    long soilNumber        = getMappedResult(seedNumber,        seedToSoilMap            );
                    long fertilizerNumber  = getMappedResult(soilNumber,        soilToFertilizerMap      );
                    long waterNumber       = getMappedResult(fertilizerNumber,  fertilizerToWaterMap     );
                    long lightNumber       = getMappedResult(waterNumber,       waterToLightMap          );
                    long temperatureNumber = getMappedResult(lightNumber,       lightToTemperatureMap    );
                    long humidityNumber    = getMappedResult(temperatureNumber, temperatureToHumidityMap );
                    long locationNumber    = getMappedResult(humidityNumber,    humidityToLocationMap    );

                    // Create the structure:
                    MappingResult mappingResult = new MappingResult(seedNumber, soilNumber, fertilizerNumber, waterNumber, lightNumber, temperatureNumber, humidityNumber, locationNumber);

                    // Check if this is the smallest so far:
                    if (locationNumber < minLocationNumber)
                    {
                        minLocationNumber = locationNumber;
                        System.out.println("min mappingResult = " + mappingResult);
                    }
                }
            }

            // Find the smallest location:

            return Long.toString(minLocationNumber);
        }

        private long getMappedResult(long input, TreeMap<Long, MapInfo> map)
        {
            // Find the first entry closest to the value:
            Map.Entry<Long, MapInfo> entry = map.floorEntry(input);
            if (entry == null) return input;

            // Unpack the entry:
            Long mapInfoKey = entry.getKey();
            MapInfo mapInfoValue = entry.getValue();

            // Work out the offset from the entry:
            long offset = input - mapInfoValue.sourceRangeStart;

            // Make sure the entry is in range:
            if (offset < mapInfoValue.rangeLength)
            {
                // We are within range.

                // Get the destination result:
                long result = mapInfoValue.destinationRangeStart() + offset;
                return result;
            }

            return input;
        }

        private TreeMap<Long, MapInfo> indexMapInfoBySourceStart(GroupSplits seedToSoilGroup)
        {
            TreeMap<Long, MapInfo> result = new TreeMap<>();
            seedToSoilGroup.numbers.forEach(mi -> result.put(mi.sourceRangeStart, mi));
            return result;
        }

    }

}
