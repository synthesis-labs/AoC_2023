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

    public int Part2(ArrayList<String> gameRecords) {
        int powerOfSetsSum = 0;

        for (String game : gameRecords) {
            String _gameID = "gameID";
            String _sets = "sets";
            int minRedNeeded = 0;
            int minGreenNeeded = 0;
            int minBlueNeeded = 0;

            int gameID = -1;
            String setsString = "";
            boolean gamePossible = true;

            Pattern pattern_GameID_Sets = Pattern.compile("^Game (?<" + _gameID + ">\\d+):(?<" + _sets + ">.*)");
            Matcher matcher_GameID_Sets = pattern_GameID_Sets.matcher(game);

            if (matcher_GameID_Sets.find()) {
                gameID = Integer.parseInt(matcher_GameID_Sets.group(_gameID));
                setsString = matcher_GameID_Sets.group(_sets);

                String[] setsArray = setsString.split(";");

                for (String set : setsArray) {
                    String _red = "red";
                    String _green = "green";
                    String _blue = "blue";

                    Pattern pattern_redCubes = Pattern.compile(".*?(?<" + _red + ">\\d+)\\s" + _red);
                    Pattern pattern_greenCubes = Pattern.compile(".*?(?<" + _green + ">\\d+)\\s" + _green);
                    Pattern pattern_blueCubes = Pattern.compile(".*?(?<" + _blue + ">\\d+)\\s" + _blue);

                    Matcher matcher_redCubes = pattern_redCubes.matcher(set);
                    Matcher matcher_greenCubes = pattern_greenCubes.matcher(set);
                    Matcher matcher_blueCubes = pattern_blueCubes.matcher(set);

                    int redCubes = matcher_redCubes.find() ? Integer.parseInt(matcher_redCubes.group(_red)) : 0;
                    int greenCubes = matcher_greenCubes.find() ? Integer.parseInt(matcher_greenCubes.group(_green)) : 0;
                    int blueCubes = matcher_blueCubes.find() ? Integer.parseInt(matcher_blueCubes.group(_blue)) : 0;

                    minRedNeeded = Math.max(redCubes, minRedNeeded);
                    minGreenNeeded = Math.max(greenCubes, minGreenNeeded);
                    minBlueNeeded = Math.max(blueCubes, minBlueNeeded);
                }
            }
            int powerOfSet = minRedNeeded * minGreenNeeded * minBlueNeeded;

            powerOfSetsSum += powerOfSet;
        }

        return powerOfSetsSum;
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
    public HashMap<Integer, HashMap<Integer, Integer>> findNumberCoordinates(ArrayList<String> schema) {
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

    public List<Integer> findPartNumbers(ArrayList<String> schema, HashMap<Integer, HashMap<Integer, Integer>> numberCoordinates) {

        List<Integer> partNumbers = new ArrayList<>();

        Pattern symbolPattern = Pattern.compile("[^\\\\.a-zA-Z\\d]");


        // For each row coordinate:
        for (int row = 0; row < numberCoordinates.size(); row++) {

            if (numberCoordinates.containsKey(row)) {

                HashMap<Integer, Integer> thisRowCoordinates = numberCoordinates.get(row);

                // Get the row from the schema:
                String thisSchematicLine = schema.get(row);


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
                            String previousSchematicLine = schema.get(previousRow);

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
                            String nextSchematicLine = schema.get(nextRow);

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
}
