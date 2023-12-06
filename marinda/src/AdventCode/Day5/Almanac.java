package AdventCode.Day5;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * The almanac that lists all of the seeds that need to be planted. It also lists what type of soil to use with each kind of seed, what type of fertilizer to
 * use with each kind of soil, what type of water to use with each kind of fertilizer, and so on. Every type of seed, soil, fertilizer and so on is identified with a number,
 * but numbers are reused by each category - that is, soil 123 and fertilizer 123 aren't necessarily related to each other.
 */
public class Almanac {
    /**
     * The list of seeds.
     */
    public List<Integer> seeds = new ArrayList<>();

    /**
     * The names of the categories in the almanac.
     */
    public enum CategoryName {
        SEED,
        SOIL,
        FERTILIZER,
        WATER,
        LIGHT,
        TEMPERATURE,
        HUMIDITY,
        LOCATION
    }

    /**
     * The names of the category maps in the almanac.
     */
    public enum CategoryMapName {
        SEED_TO_SOIL,
        SOIL_TO_FERTILIZER,
        FERTILIZER_TO_WATER,
        WATER_TO_LIGHT,
        LIGHT_TO_TEMPERATURE,
        TEMPERATURE_TO_HUMIDITY,
        HUMIDITY_TO_LOCATION
    }

    /**
     * A list of category maps.
     * A category map is a map of one source category to one destination category.
     * It describes entire ranges of numbers that can be converted.
     */
    public Map<CategoryMapName, List<CategoryMapEntry>> categoryMaps = new HashMap<>();

    public Almanac() {
        // Set up the category maps:
        for (CategoryMapName mapName : CategoryMapName.values()) {
            categoryMaps.put(mapName, new ArrayList<>());
        }
    }
}
