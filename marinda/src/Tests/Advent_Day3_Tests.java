package Tests;

import AdventCode.Advent_Day3;
import org.junit.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;

import static org.junit.Assert.assertEquals;

public class Advent_Day3_Tests {

    /**
     *
     */
    //region Part 1:
    @Test
    public void Part1_Test_SampleData() {

        Advent_Day3 Advent_Day3 = new Advent_Day3();

        ArrayList<String> sampleData = new ArrayList<>();
        sampleData.add("");
        sampleData.add("");
        sampleData.add("");
        sampleData.add("");
        sampleData.add("");

        int possibleGameIDsSum = Advent_Day3.Part1(sampleData);

        assertEquals(8, possibleGameIDsSum);
    }

    @Test
    public void Part1_Test_RealData() throws IOException {
        Advent_Day3 Advent_Day3 = new Advent_Day3();

        ArrayList<String> data = (ArrayList<String>) Files.readAllLines(Paths.get("./src/Data/Advent_Day3_Data.txt"));

        int possibleGameIDsSum = Advent_Day3.Part1(data);

        assertEquals(2176, possibleGameIDsSum);
    }
    //endregion

    /**
     *
     */
    //region Part 2:
    @Test
    public void Part2_Test_SampleData() {
        Advent_Day3 Advent_Day3 = new Advent_Day3();

        ArrayList<String> sampleData = new ArrayList<String>();
        sampleData.add("");
        sampleData.add("");
        sampleData.add("");
        sampleData.add("");
        sampleData.add("");

        int powerOfSetsSum = Advent_Day3.Part2(sampleData);

        assertEquals(2286, powerOfSetsSum);
    }

    @Test
    public void Part2_Test_RealData() throws IOException {
        Advent_Day3 Advent_Day3 = new Advent_Day3();

        ArrayList<String> data = (ArrayList<String>) Files.readAllLines(Paths.get("./src/Data/Advent_Day3_Data.txt"));

        int powerOfSetsSum = Advent_Day3.Part2(data);

        assertEquals(63700, powerOfSetsSum);
    }
    //endregion
}
