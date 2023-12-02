package Tests;

import AdventCode.Advent_Day2;
import AdventCode.Advent_Day2;
import org.junit.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;

import static org.junit.Assert.assertEquals;

public class Advent_Day2_Tests {

    @Test
    public void Day2_Part1_Test_SampleData(){
        Advent_Day2 advent_day2 = new Advent_Day2();

        ArrayList<String> sampleData = new ArrayList<String>();
        sampleData.add("1abc2");
        sampleData.add("pqr3stu8vwx");
        sampleData.add("a1b2c3d4e5f");
        sampleData.add("treb7uchet");

        int calibrationValuesSum = advent_day2.Part1(sampleData);

        assertEquals(142, calibrationValuesSum);
    }

    @Test
    public void Day2_Part1_Test_RealData() throws IOException {
        Advent_Day2 advent_day2 = new Advent_Day2();

        ArrayList<String> data = (ArrayList<String>) Files.readAllLines(Paths.get("./src/Data/Advent_Day_1_Data.txt"));

        int calibrationValuesSum = advent_day2.Part1(data);

        assertEquals(54338, calibrationValuesSum);
    }

    @Test
    public void Day2_Part2_Test_SampleData(){
        Advent_Day2 advent_day_1 = new Advent_Day2();

        ArrayList<String> sampleData = new ArrayList<String>();
        sampleData.add("two1nine");
        sampleData.add("eightwothree");
        sampleData.add("abcone2threexyz");
        sampleData.add("xtwone3four");
        sampleData.add("4nineeightseven2");
        sampleData.add("zoneight234");
        sampleData.add("7pqrstsixteen");

        int calibrationValuesSum = advent_day_1.Part2(sampleData);

        assertEquals(281, calibrationValuesSum);
    }

    @Test
    public void Day2_Part2_Test_RealData() throws IOException {
        Advent_Day2 advent_day_1 = new Advent_Day2();

        ArrayList<String> data = (ArrayList<String>) Files.readAllLines(Paths.get("./src/Data/Advent_Day_1_Data.txt"));

        int calibrationValuesSum = advent_day_1.Part2(data);

        assertEquals(54338, calibrationValuesSum);
    }
}
