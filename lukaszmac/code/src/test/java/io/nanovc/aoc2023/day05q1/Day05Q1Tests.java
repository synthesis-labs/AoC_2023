package io.nanovc.aoc2023.day05q1;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 *  --- Day 5: If You Give A Seed A Fertilizer ---
 * You take the boat and find the gardener right where you were told he would be: managing a giant "garden" that looks more to you like a farm.
 *
 * "A water source? Island Island is the water source!" You point out that Snow Island isn't receiving any water.
 *
 * "Oh, we had to stop the water because we ran out of sand to filter it with! Can't make snow with dirty water. Don't worry, I'm sure we'll get more sand soon; we only turned off the water a few days... weeks... oh no." His face sinks into a look of horrified realization.
 *
 * "I've been so busy making sure everyone here has food that I completely forgot to check why we stopped getting more sand! There's a ferry leaving soon that is headed over in that direction - it's much faster than your boat. Could you please go check it out?"
 *
 * You barely have time to agree to this request when he brings up another. "While you wait for the ferry, maybe you can help us with our food production problem. The latest Island Island Almanac just arrived and we're having trouble making sense of it."
 *
 * The almanac (your puzzle input) lists all of the seeds that need to be planted. It also lists what type of soil to use with each kind of seed, what type of fertilizer to use with each kind of soil, what type of water to use with each kind of fertilizer, and so on. Every type of seed, soil, fertilizer and so on is identified with a number, but numbers are reused by each category - that is, soil 123 and fertilizer 123 aren't necessarily related to each other.
 *
 * For example:
 *
 * seeds: 79 14 55 13
 *
 * seed-to-soil map:
 * 50 98 2
 * 52 50 48
 *
 * soil-to-fertilizer map:
 * 0 15 37
 * 37 52 2
 * 39 0 15
 *
 * fertilizer-to-water map:
 * 49 53 8
 * 0 11 42
 * 42 0 7
 * 57 7 4
 *
 * water-to-light map:
 * 88 18 7
 * 18 25 70
 *
 * light-to-temperature map:
 * 45 77 23
 * 81 45 19
 * 68 64 13
 *
 * temperature-to-humidity map:
 * 0 69 1
 * 1 0 69
 *
 * humidity-to-location map:
 * 60 56 37
 * 56 93 4
 * The almanac starts by listing which seeds need to be planted: seeds 79, 14, 55, and 13.
 *
 * The rest of the almanac contains a list of maps which describe how to convert numbers from a source category into numbers in a destination category. That is, the section that starts with seed-to-soil map: describes how to convert a seed number (the source) to a soil number (the destination). This lets the gardener and his team know which soil to use with which seeds, which water to use with which fertilizer, and so on.
 *
 * Rather than list every source number and its corresponding destination number one by one, the maps describe entire ranges of numbers that can be converted. Each line within a map contains three numbers: the destination range start, the source range start, and the range length.
 *
 * Consider again the example seed-to-soil map:
 *
 * 50 98 2
 * 52 50 48
 * The first line has a destination range start of 50, a source range start of 98, and a range length of 2. This line means that the source range starts at 98 and contains two values: 98 and 99. The destination range is the same length, but it starts at 50, so its two values are 50 and 51. With this information, you know that seed number 98 corresponds to soil number 50 and that seed number 99 corresponds to soil number 51.
 *
 * The second line means that the source range starts at 50 and contains 48 values: 50, 51, ..., 96, 97. This corresponds to a destination range starting at 52 and also containing 48 values: 52, 53, ..., 98, 99. So, seed number 53 corresponds to soil number 55.
 *
 * Any source numbers that aren't mapped correspond to the same destination number. So, seed number 10 corresponds to soil number 10.
 *
 * So, the entire list of seed numbers and their corresponding soil numbers looks like this:
 *
 * seed  soil
 * 0     0
 * 1     1
 * ...   ...
 * 48    48
 * 49    49
 * 50    52
 * 51    53
 * ...   ...
 * 96    98
 * 97    99
 * 98    50
 * 99    51
 * With this map, you can look up the soil number required for each initial seed number:
 *
 * Seed number 79 corresponds to soil number 81.
 * Seed number 14 corresponds to soil number 14.
 * Seed number 55 corresponds to soil number 57.
 * Seed number 13 corresponds to soil number 13.
 * The gardener and his team want to get started as soon as possible, so they'd like to know the closest location that needs a seed. Using these maps, find the lowest location number that corresponds to any of the initial seeds. To do this, you'll need to convert each seed number through other categories until you can find its corresponding location number. In this example, the corresponding types are:
 *
 * Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78, humidity 78, location 82.
 * Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42, humidity 43, location 43.
 * Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82, humidity 82, location 86.
 * Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.
 * So, the lowest location number in this example is 35.
 *
 * What is the lowest location number that corresponds to any of the initial seed numbers?
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2023/day/3">Challenge</a>
 */
public abstract class Day05Q1Tests extends TestBase
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
        return "Day 05 Q1";
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
        return "35";
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

    public static class Solution1Tests extends Day05Q1Tests
    {
        public record GroupText(String groupName, String text) {}
        public record MapInfo(long destinationRangeStart, long sourceRangeStart, long rangeLength) {}
        public record GroupSplits(String groupName, List<MapInfo> numbers) {}

        public record Seed(long seedNumber) {}

        public record MappingResult(
                Seed seed,
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

            // Parse the seeds:
            List<Seed> seedList = Arrays.stream(seedInfo.split(": ")[1].split("\\s+")).map(Long::parseLong).map(Seed::new).toList();

            // Create a map of the seeds:
            Map<Long, Seed> seedMap = new HashMap<>();
            seedList.forEach(seed -> seedMap.put(seed.seedNumber(), seed));

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


            // Initialise the seed to location map with defaults:
            Map<Long, Long> seedToLocationMap = new HashMap<>();
            Map<Long, MappingResult> seedMappingInfo = new HashMap<>();
            seedList.forEach(s -> seedToLocationMap.put(s.seedNumber(), s.seedNumber()));
            // Now we have all un-mapped seeds going to their same seed number location.

            // Keep track of the minimum locationNumber:
            long minLocationNumber = Long.MAX_VALUE;

            // Process each seed through the mappings:
            for (Seed seed : seedList)
            {
                // Get details:
                long seedNumber = seed.seedNumber();

                // Map the entries:
                long soilNumber        = getMappedResult(seedNumber,        seedToSoilMap            );
                long fertilizerNumber  = getMappedResult(soilNumber,        soilToFertilizerMap      );
                long waterNumber       = getMappedResult(fertilizerNumber,  fertilizerToWaterMap     );
                long lightNumber       = getMappedResult(waterNumber,       waterToLightMap          );
                long temperatureNumber = getMappedResult(lightNumber,       lightToTemperatureMap    );
                long humidityNumber    = getMappedResult(temperatureNumber, temperatureToHumidityMap );
                long locationNumber    = getMappedResult(humidityNumber,    humidityToLocationMap    );

                // Create the structure:
                MappingResult mappingResult = new MappingResult(seed, seedNumber, soilNumber, fertilizerNumber, waterNumber, lightNumber, temperatureNumber, humidityNumber, locationNumber);
                System.out.println("mappingResult = " + mappingResult);

                // Save the mapping:
                seedToLocationMap.put(seedNumber, locationNumber);
                seedMappingInfo.put(seedNumber, mappingResult);

                // Check if this is the smallest so far:
                if (locationNumber < minLocationNumber)
                {
                    minLocationNumber = locationNumber;
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
