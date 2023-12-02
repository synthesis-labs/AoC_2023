package Tests;

import AdventCode.Advent_Day_1;
import org.junit.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Advent_Day_1_Tests {

    @Test
    public void Day1_Solution1_Test_SampleData(){
        Advent_Day_1 advent_day_1 = new Advent_Day_1();

        ArrayList<String> sampleData = new ArrayList<String>();
        sampleData.add("1abc2");
        sampleData.add("pqr3stu8vwx");
        sampleData.add("a1b2c3d4e5f");
        sampleData.add("treb7uchet");

        int calibrationValuesSum = advent_day_1.Day1_Solution1(sampleData);

        assertEquals(142, calibrationValuesSum);
    }

    @Test
    public void Day1_Solution1_Test_RealData() throws IOException {
        Advent_Day_1 advent_day_1 = new Advent_Day_1();

        ArrayList<String> data = (ArrayList<String>) Files.readAllLines(Paths.get("./src/Data/Advent_Day_1_Data.txt"));

        int calibrationValuesSum = advent_day_1.Day1_Solution1(data);

        assertEquals(54338, calibrationValuesSum);
    }
}
