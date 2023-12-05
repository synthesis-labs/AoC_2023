package AdventCode;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Advent_Day3 {

    public Advent_Day3() {
    }

    public int Part1(ArrayList<String> engineSchema) {

        HashMap<Integer, HashMap<Integer, Integer>> numberCoordinates = findNumberCoordinates(engineSchema);
        List<Integer> partNumbers = findPartNumbers(engineSchema, numberCoordinates);

        int partNumbersSum = partNumbers.stream().mapToInt(Integer::intValue).sum();

        return partNumbersSum;
    }

    public int Part2(ArrayList<String> engineSchema) {

        HashMap<Integer, ArrayList<Integer>> asteriskCoordinates = findAsteriskCoordinates(engineSchema);
        List<Integer> gearRatios = findGearRatios(engineSchema, asteriskCoordinates);

        int gearRatiosSum = gearRatios.stream().mapToInt(Integer::intValue).sum();

        return gearRatiosSum;
    }


    public HashMap<String, String> findNumbersAdjacentToSymbolHorizontal(String schematicLine) {
        String _num = "num";
        String numbers = "";

        HashMap<String, String> startIndexToNumberMap = new HashMap<>();

        Pattern patternSymbolBefore = Pattern.compile("(\\D*)((?<![\\.a-zA-Z])(?<num>\\b\\d+))");
        Pattern patternSymbolAfter = Pattern.compile("[\\.a-zA-Z]*(?<num>\\d+)\\b(?![\\.a-zA-Z])");

        Matcher matcherSymbolBefore = patternSymbolBefore.matcher(schematicLine);
        Matcher matcherSymbolAfter = patternSymbolAfter.matcher(schematicLine);

        StringBuilder sb = new StringBuilder(numbers);

        int matchCount = 0;

        while (matcherSymbolBefore.find()) {
            matchCount++;
            String number = matcherSymbolBefore.group(_num);
            String start = String.valueOf(matcherSymbolBefore.start(_num));

            startIndexToNumberMap.put(start, number);
        }

        while (matcherSymbolAfter.find()) {
            String number = matcherSymbolAfter.group(_num);
            String start = String.valueOf(matcherSymbolAfter.start(_num));

            startIndexToNumberMap.put(start, number);
        }

        return startIndexToNumberMap;
    }


    /**
     * Find the numbers and their start and end coordinates.
     * <p>
     * HashMap <row, Hashmap<start, end?>
     *
     * @param schema
     * @return
     */
    public HashMap<Integer, HashMap<Integer, Integer>> findNumberCoordinates(List<String> schema) {
        String _num = "num";

        HashMap<Integer, HashMap<Integer, Integer>> numberCoordinates = new HashMap<>();
        for (int row = 0; row < schema.size(); row++) {

            String schematicLine = schema.get(row);

            Pattern numberPattern = Pattern.compile("\\D?(?<num>\\b\\d+)\\D?");
            Matcher numberMatcher = numberPattern.matcher(schematicLine);

            numberCoordinates.putIfAbsent(row, new HashMap<>());

            while (numberMatcher.find()) {
                String number = numberMatcher.group(_num);
                int startIndexInclusive = numberMatcher.start(_num);
                int endIndexExclusive = numberMatcher.end(_num);

                numberCoordinates.get(row).putIfAbsent(startIndexInclusive, endIndexExclusive);
            }

        }

        return numberCoordinates;

    }

    /**
     * Find the asterisk coordinates.
     * <p>
     * HashMap <row, Hashmap<start, end?>
     *
     * @param schema
     * @return
     */
    public HashMap<Integer, ArrayList<Integer>> findAsteriskCoordinates(ArrayList<String> schema) {
        String _gear = "gear";

        HashMap<Integer, ArrayList<Integer>> gearCoordinates = new HashMap<>();

        for (int row = 0; row < schema.size(); row++) {

            String schematicLine = schema.get(row);

            Pattern asteriskPattern = Pattern.compile("[^\\*]*(?<gear>\\*)[^\\*]*?");
            Matcher asteriskMatcher = asteriskPattern.matcher(schematicLine);


            while (asteriskMatcher.find()) {
                gearCoordinates.putIfAbsent(row, new ArrayList<>());

                int startIndexInclusive = asteriskMatcher.start(_gear);

                gearCoordinates.get(row).add(startIndexInclusive);
            }

        }

        return gearCoordinates;
    }

    public List<Integer> findPartNumbers(ArrayList<String> engineSchema, HashMap<Integer, HashMap<Integer, Integer>> numberCoordinates) {

        List<Integer> partNumbers = new ArrayList<>();

        Pattern symbolPattern = Pattern.compile("[^\\\\.a-zA-Z\\d]");


        // For each row coordinate:
        for (int row = 0; row < numberCoordinates.size(); row++) {

            if (numberCoordinates.containsKey(row)) {

                HashMap<Integer, Integer> thisRowCoordinates = numberCoordinates.get(row);

                // Get the row from the schema:
                String thisSchematicLine = engineSchema.get(row);


                // For each start coordinate:
                for (int startIndexInclusive = 0; startIndexInclusive < thisSchematicLine.length(); startIndexInclusive++) {

                    if (thisRowCoordinates.containsKey(startIndexInclusive)) {

                        boolean isPartNumber = false;

                        // Get the end coordinate:
                        int endIndexExclusive = thisRowCoordinates.get(startIndexInclusive);

                        // Get the string to check:
                        int thisStart = startIndexInclusive != 0 ? startIndexInclusive - 1 : 0;
                        int thisEnd = endIndexExclusive < thisSchematicLine.length() ? endIndexExclusive : endIndexExclusive - 1;

                        String thisRowStringToCheck = thisSchematicLine.substring(thisStart, thisEnd + 1);

                        // Check for symbols on the same row:
                        Matcher symbolsMatcher = symbolPattern.matcher(thisRowStringToCheck);

                        while (symbolsMatcher.find()) {
                            isPartNumber = true;
                        }

                        // If no symbols on this line, check previous line:
                        int previousRow = row - 1;
                        if (!isPartNumber && numberCoordinates.containsKey(previousRow)) {

                            // Get the row from the schema:
                            String previousSchematicLine = engineSchema.get(previousRow);

                            // Get the string to check:
                            int previousStart = startIndexInclusive - 1 > 0 ? startIndexInclusive - 1 : 0;
                            int previousEnd = endIndexExclusive < thisSchematicLine.length() ? endIndexExclusive : endIndexExclusive - 1;

                            String previousRowStringToCheck = previousSchematicLine.substring(previousStart, previousEnd + 1);

                            // check for symbols:
                            symbolsMatcher = symbolPattern.matcher(previousRowStringToCheck);

                            while (symbolsMatcher.find()) {
                                isPartNumber = true;
                            }
                        }

                        // If no symbols on this line, check previous line:
                        int nextRow = row + 1;
                        if (!isPartNumber && numberCoordinates.containsKey(nextRow)) {

                            // Get the row from the schema:
                            String nextSchematicLine = engineSchema.get(nextRow);

                            // Get the string to check:
                            int nextStart = startIndexInclusive - 1 > 0 ? startIndexInclusive - 1 : 0;
                            int nextEnd = endIndexExclusive < thisSchematicLine.length() ? endIndexExclusive : endIndexExclusive - 1;

                            String nextRowStringToCheck = nextSchematicLine.substring(nextStart, nextEnd + 1);

                            // check for symbols:
                            symbolsMatcher = symbolPattern.matcher(nextRowStringToCheck);

                            while (symbolsMatcher.find()) {
                                isPartNumber = true;
                            }
                        }

                        // Check if this is a part number:
                        if (isPartNumber) {
                            // Get the number:
                            int partNumber = Integer.parseInt(thisSchematicLine.substring(startIndexInclusive, endIndexExclusive));

                            // Add partNumber to list:
                            partNumbers.add(partNumber);
                        }
                    }
                }
            }
        }

        return partNumbers;
    }


    private List<Integer> findGearRatios(List<String> engineSchema, HashMap<Integer, ArrayList<Integer>> asteriskCoordinates) {

        List<Integer> gearRatios = new ArrayList<>();

//        Pattern symbolPattern = Pattern.compile("[^\\\\.a-zA-Z\\d]");


        // For each row coordinate:
        for (int thisRow = 0; thisRow < engineSchema.size(); thisRow++) {

            if (asteriskCoordinates.containsKey(thisRow)) {

                // Part numbers can be in previous row, next row or same row.
                int previousRow = thisRow - 1;
                int nextRow = thisRow + 1;

                ArrayList<Integer> thisRowCoordinates = asteriskCoordinates.get(thisRow);

                // Get the row from the schema:
                String thisSchematicLine = engineSchema.get(thisRow);
                String previousSchematicLine = previousRow > -1 ? engineSchema.get(previousRow) : null;
                String nextSchematicLine = nextRow < engineSchema.size() ? engineSchema.get(nextRow) : null;

                // For each start coordinate:
                for (int thisAsteriskCoordinate : thisRowCoordinates) {

                    List<Integer> thisGearNums = new ArrayList<>();

                    // Look for part numbers around asterisk:
                    Pattern numberPattern = Pattern.compile("\\D?(?<num>\\b\\d+)\\D?");


                    // this line:
                    Matcher thisLineMatcher = numberPattern.matcher(thisSchematicLine);

                    while (thisLineMatcher.find()) {
                        int num = Integer.parseInt(thisLineMatcher.group("num"));

                        int numStart = thisLineMatcher.start("num");
                        int numEnd = thisLineMatcher.end("num");

                        // Check if this number is next to our coordinate:

                        boolean numToLeft = numEnd == thisAsteriskCoordinate;
                        boolean numToRight = numStart == thisAsteriskCoordinate + 1;

                        if (numToLeft || numToRight) {
                            thisGearNums.add(num);
                        }
                    }

                    // previous line:
                    if (previousSchematicLine != null) {
                        Matcher previousLineMatcher = numberPattern.matcher(previousSchematicLine);

                        while (previousLineMatcher.find()) {
                            int num = Integer.parseInt(previousLineMatcher.group("num"));

                            int numStart = previousLineMatcher.start("num");
                            int numEnd = previousLineMatcher.end("num");

                            // Check if this number is above our coordinate:

                            boolean numAboveToLeft = numEnd == thisAsteriskCoordinate;
                            boolean numAboveToRight = numStart == thisAsteriskCoordinate + 1;
                            boolean numAboveCenter = thisAsteriskCoordinate >= numStart && thisAsteriskCoordinate < numEnd;

                            if (numAboveToLeft || numAboveToRight || numAboveCenter) {
                                thisGearNums.add(num);
                            }
                        }
                    }

                    // next line:
                    if (nextSchematicLine != null) {
                        Matcher nextLineMatcher = numberPattern.matcher(nextSchematicLine);

                        while (nextLineMatcher.find()) {
                            int num = Integer.parseInt(nextLineMatcher.group("num"));

                            int numStart = nextLineMatcher.start("num");
                            int numEnd = nextLineMatcher.end("num");

                            // Check if this number is below our coordinate:

                            boolean numBelowToLeft = numEnd == thisAsteriskCoordinate;
                            boolean numBelowToRight = numStart == thisAsteriskCoordinate + 1;
                            boolean numBelowCenter = thisAsteriskCoordinate >= numStart && thisAsteriskCoordinate < numEnd;

                            if (numBelowToLeft || numBelowToRight || numBelowCenter) {
                                thisGearNums.add(num);
                            }
                        }
                    }

                    // Check if this is a gear ratio and multiply the two nums to get the gear ratio:
                    if (thisGearNums.size() == 2) {
                        int gearRatio = thisGearNums.get(0) * thisGearNums.get(1);
                        gearRatios.add(gearRatio);
                    }
                }
            }
        }

        return gearRatios;
    }
}
