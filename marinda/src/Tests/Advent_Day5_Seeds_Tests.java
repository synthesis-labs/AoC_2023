package Tests;

import AdventCode.Day5.Advent_Day5_Seeds;
import AdventCode.Day5.Almanac;
import AdventCode.Day5.CategoryMapEntry;
import org.junit.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;

public class Advent_Day5_Seeds_Tests {

    /**
     * --- Day 5: If You Give A Seed A Fertilizer ---
     * <p>
     * You take the boat and find the gardener right where you were told he would be: managing a giant "garden" that looks more to you like a farm.
     * <p>
     * "A water source? Island Island is the water source!" You point out that Snow Island isn't receiving any water.
     * <p>
     * "Oh, we had to stop the water because we ran out of sand to filter it with! Can't make snow with dirty water. Don't worry, I'm sure we'll get more sand soon; we only
     * turned off the water a few days... weeks... oh no." His face sinks into a look of horrified realization.
     * <p>
     * "I've been so busy making sure everyone here has food that I completely forgot to check why we stopped getting more sand! There's a ferry leaving soon that is headed over
     * in that direction - it's much faster than your boat. Could you please go check it out?"
     * <p>
     * You barely have time to agree to this request when he brings up another. "While you wait for the ferry, maybe you can help us with our food production problem. The latest
     * Island Island Almanac just arrived and we're having trouble making sense of it."
     * <p>
     * The almanac (your puzzle input) lists all of the seeds that need to be planted. It also lists what type of soil to use with each kind of seed, what type of fertilizer to
     * use with each kind of soil, what type of water to use with each kind of fertilizer, and so on. Every type of seed, soil, fertilizer and so on is identified with a number,
     * but numbers are reused by each category - that is, soil 123 and fertilizer 123 aren't necessarily related to each other.
     * <p>
     * For example:
     * <p>
     * seeds: 79 14 55 13
     * <p>
     * seed-to-soil map:
     * 50 98 2
     * 52 50 48
     * <p>
     * soil-to-fertilizer map:
     * 0 15 37
     * 37 52 2
     * 39 0 15
     * <p>
     * fertilizer-to-water map:
     * 49 53 8
     * 0 11 42
     * 42 0 7
     * 57 7 4
     * <p>
     * water-to-light map:
     * 88 18 7
     * 18 25 70
     * <p>
     * light-to-temperature map:
     * 45 77 23
     * 81 45 19
     * 68 64 13
     * <p>
     * temperature-to-humidity map:
     * 0 69 1
     * 1 0 69
     * <p>
     * humidity-to-location map:
     * 60 56 37
     * 56 93 4
     * <p>
     * The almanac starts by listing which seeds need to be planted: seeds 79, 14, 55, and 13.
     * <p>
     * The rest of the almanac contains a list of maps which describe how to convert numbers from a source category into numbers in a destination category. That is, the section
     * that starts with seed-to-soil map: describes how to convert a seed number (the source) to a soil number (the destination). This lets the gardener and his team know which
     * soil to use with which seeds, which water to use with which fertilizer, and so on.
     * <p>
     * Rather than list every source number and its corresponding destination number one by one, the maps describe entire ranges of numbers that can be converted. Each line
     * within a map contains three numbers: the destination range start, the source range start, and the range length.
     * <p>
     * Consider again the example seed-to-soil map:
     * <p>
     * 50 98 2
     * 52 50 48
     * <p>
     * The first line has a destination range start of 50, a source range start of 98, and a range length of 2. This line means that the source range starts at 98 and contains
     * two values: 98 and 99. The destination range is the same length, but it starts at 50, so its two values are 50 and 51. With this information, you know that seed number 98
     * corresponds to soil number 50 and that seed number 99 corresponds to soil number 51.
     * <p>
     * The second line means that the source range starts at 50 and contains 48 values: 50, 51, ..., 96, 97. This corresponds to a destination range starting at 52 and also
     * containing 48 values: 52, 53, ..., 98, 99. So, seed number 53 corresponds to soil number 55.
     * <p>
     * Any source numbers that aren't mapped correspond to the same destination number. So, seed number 10 corresponds to soil number 10.
     * <p>
     * So, the entire list of seed numbers and their corresponding soil numbers looks like this:
     * <p>
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
     * <p>
     * With this map, you can look up the soil number required for each initial seed number:
     * <p>
     * Seed number 79 corresponds to soil number 81.
     * Seed number 14 corresponds to soil number 14.
     * Seed number 55 corresponds to soil number 57.
     * Seed number 13 corresponds to soil number 13.
     * <p>
     * The gardener and his team want to get started as soon as possible, so they'd like to know the closest location that needs a seed. Using these maps, find the lowest
     * location number that corresponds to any of the initial seeds. To do this, you'll need to convert each seed number through other categories until you can find its
     * corresponding location number. In this example, the corresponding types are:
     * <p>
     * Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78, humidity 78, location 82.
     * Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42, humidity 43, location 43.
     * Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82, humidity 82, location 86.
     * Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.
     * <p>
     * So, the lowest location number in this example is 35.
     * <p>
     * What is the lowest location number that corresponds to any of the initial seed numbers?
     */
    //region Part 1:
    @Test
    public void Part1_Test_SampleData() {

        Advent_Day5_Seeds advent_Day5 = new Advent_Day5_Seeds();

        ArrayList<String> sampleData = new ArrayList<>();
        sampleData.add("seeds: 79 14 55 13");
        sampleData.add("");
        sampleData.add("seed-to-soil map:");
        sampleData.add("50 98 2");
        sampleData.add("52 50 48");
        sampleData.add("");
        sampleData.add("soil-to-fertilizer map:");
        sampleData.add("0 15 37");
        sampleData.add("37 52 2");
        sampleData.add("39 0 15");
        sampleData.add("");
        sampleData.add("fertilizer-to-water map:");
        sampleData.add("49 53 8");
        sampleData.add("0 11 42");
        sampleData.add("42 0 7");
        sampleData.add("57 7 4");
        sampleData.add("");
        sampleData.add("water-to-light map:");
        sampleData.add("88 18 7");
        sampleData.add("18 25 70");
        sampleData.add("");
        sampleData.add("light-to-temperature map:");
        sampleData.add("45 77 23");
        sampleData.add("81 45 19");
        sampleData.add("68 64 13");
        sampleData.add("");
        sampleData.add("temperature-to-humidity map:");
        sampleData.add("0 69 1");
        sampleData.add("1 0 69");
        sampleData.add("");
        sampleData.add("humidity-to-location map:");
        sampleData.add("60 56 37");
        sampleData.add("56 93 4");


        Almanac almanac = new Almanac();
        advent_Day5.readInAlmanacData(almanac, sampleData);

        assertEquals("[79, 14, 55, 13]", almanac.seeds.toString());

        Map<Almanac.CategoryMapName, List<CategoryMapEntry>> categoryMaps = almanac.categoryMaps;
        CategoryMapEntry seedToSoilEntry = categoryMaps.get(Almanac.CategoryMapName.SEED_TO_SOIL).get(0);

        assertEquals(50, seedToSoilEntry.destinationRangeStart);
        assertEquals(98, seedToSoilEntry.sourceRangeStart);
        assertEquals(2, seedToSoilEntry.rangeLength);

        int lowestLocationNumber = advent_Day5.Part1(sampleData);
    }

    @Test
    public void Part1_Test_RealData() throws IOException {
        Advent_Day5_Seeds advent_Day5 = new Advent_Day5_Seeds();

        ArrayList<String> data = (ArrayList<String>) Files.readAllLines(Paths.get("./src/Data/Advent_Day5_Seeds_Data.txt"));

        int scratchcardsPoints = advent_Day5.Part1(data);

        assertEquals(21568, scratchcardsPoints);
    }
    //endregion

    /**
     *
     */
    //region Part 2:
    @Test
    public void Part2_Test_SampleData() {
        Advent_Day5_Seeds advent_Day5 = new Advent_Day5_Seeds();

    }

    @Test
    public void Part2_Test_RealData() throws IOException {
        Advent_Day5_Seeds advent_Day5 = new Advent_Day5_Seeds();

        ArrayList<String> data = (ArrayList<String>) Files.readAllLines(Paths.get("./src/Data/Advent_Day5_Seeds_Data.txt"));

        int numberOfScratchcards = advent_Day5.Part2(data);

        assertEquals(11827296, numberOfScratchcards);
    }
    //endregion
}
