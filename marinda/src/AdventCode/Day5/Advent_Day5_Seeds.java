package AdventCode.Day5;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Advent_Day5_Seeds {

    public Advent_Day5_Seeds() {
    }

    public int Part1(List<String> almanacData) {
        int closestLocation = -1;

        Almanac almanac = new Almanac();
        readInAlmanacData(almanac, almanacData);

        for (int seed : almanac.seeds) {
            int locationNumber = findLocationNumberForSeed(almanac, seed);
        }


        return closestLocation;

    }

    private int findLocationNumberForSeed(Almanac almanac, int seedNum) {
        int soilNum = -1;
        int fertilizerNum = -1;


        int locationNum = -1;

        List<CategoryMapEntry> seedToSoilMap = almanac.categoryMaps.get(Almanac.CategoryMapName.SEED_TO_SOIL);
        while (soilNum < 0) {
            for (CategoryMapEntry seedToSoilEntry : seedToSoilMap) {
                if (seedNum >= seedToSoilEntry.sourceRangeStart && seedNum < seedToSoilEntry.sourceRangeEnd) {
                    int seedNumIndex = seedToSoilEntry.sourceRangeStart - seedNum;
                    soilNum = seedToSoilEntry.destinationRangeStart + seedNumIndex;
                }
            }
        }

        List<CategoryMapEntry> soilToFertilizerMap = almanac.categoryMaps.get(Almanac.CategoryMapName.SOIL_TO_FERTILIZER);
        fertilizerNum = 0;
        while (fertilizerNum < 0) {
            for (CategoryMapEntry soilToFertilizerEntry : soilToFertilizerMap) {
                if (soilNum >= soilToFertilizerEntry.sourceRangeStart && seedNum < soilToFertilizerEntry.sourceRangeEnd) {
                    int seedNumIndex = soilToFertilizerEntry.sourceRangeStart - seedNum;
                    fertilizerNum = soilToFertilizerEntry.destinationRangeStart + seedNumIndex;
                }
            }
        }


        return locationNum;
    }

    public int Part2(ArrayList<String> almanacData) {
        int closestLocatoion = -1;
        return closestLocatoion;

    }

    public void readInAlmanacData(Almanac almanac, List<String> almanacData) {
        Pattern seedListPattern = Pattern.compile("(\\s*(?<seed>\\d+)\\s*)");
        Pattern mapNamePattern = Pattern.compile("(?<mapName>.*)\\smap:");
        Pattern mapEntryPattern = Pattern.compile("(?<destinationRangeStart>\\d*)\\s(?<sourceRangeStart>\\d*)\\s(?<rangeLength>\\d*)");

        for (int rowIndex = 0; rowIndex < almanacData.size(); rowIndex++) {
            String almanacRow = almanacData.get(rowIndex);

            // Understand this row:

            // Check for seed row:
            if (almanacRow.contains("seeds")) {
                String seedsString = almanacRow.split(":")[1];

                Matcher seedListMatcher = seedListPattern.matcher(seedsString);

                while (seedListMatcher.find()) {
                    int seed = Integer.parseInt(seedListMatcher.group("seed"));
                    almanac.seeds.add(seed);
                }
            }

            // Check for map heading row:
            if (almanacRow.contains("map")) {
                // This is a map heading. Get the map name:
                Matcher mapNameMatcher = mapNamePattern.matcher(almanacRow);

                if (mapNameMatcher.find()) {
                    String mapName = mapNameMatcher.group("mapName");
                    mapName = mapName.replace("-", "_").toUpperCase();
                    Almanac.CategoryMapName categoryMapName = Almanac.CategoryMapName.valueOf(mapName);

                    // Read the entries for this map:
                    while (!almanacRow.isBlank() && rowIndex + 1 < almanacData.size()) {
                        rowIndex++;
                        almanacRow = almanacData.get(rowIndex);

                        Matcher mapEntryMatcher = mapEntryPattern.matcher(almanacRow);

                        if (mapEntryMatcher.find()) {
                            int destination = Integer.parseInt(mapEntryMatcher.group("destinationRangeStart"));
                            int source = Integer.parseInt(mapEntryMatcher.group("sourceRangeStart"));
                            int range = Integer.parseInt(mapEntryMatcher.group("rangeLength"));

                            // Create a new category map entry:
                            CategoryMapEntry categoryMapEntry = new CategoryMapEntry(destination, source, range);

                            // Add the map entry to the map:
                            almanac.categoryMaps.get(categoryMapName).add(categoryMapEntry);
                        }
                    }
                }
            }
        }

    }
}
