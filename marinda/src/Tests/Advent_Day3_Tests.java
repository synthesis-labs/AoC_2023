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
     * --- Day 3: Gear Ratios ---
     * <p>
     * You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.
     * <p>
     * It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.
     * <p>
     * "Aaah!"
     * <p>
     * You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.
     * <p>
     * The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.
     * <p>
     * The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)
     * <p>
     * Here is an example engine schematic:
     * <p>
     * 467..114..
     * ...*......
     * ..35..633.
     * ......#...
     * 617*......
     * .....+.58.
     * ..592.....
     * ......755.
     * ...$.*....
     * .664.598..
     * <p>
     * In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.
     * <p>
     * Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?
     */
    //region Part 1:
    @Test
    public void Part1_Test_SampleData() {

        Advent_Day3 advent_Day3 = new Advent_Day3();

        ArrayList<String> sampleData = new ArrayList<>();
        sampleData.add("467..114..");
        sampleData.add("...*......");
        sampleData.add("..35..633.");
        sampleData.add("......#...");
        sampleData.add("617*......");
        sampleData.add(".....+.58.");
        sampleData.add("..592.....");
        sampleData.add("......755.");
        sampleData.add("...$.*....");
        sampleData.add(".664.598..");


        int partNumbersSum = advent_Day3.Part1(sampleData);

        assertEquals(4361, partNumbersSum);
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
