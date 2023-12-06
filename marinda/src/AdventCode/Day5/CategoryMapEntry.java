package AdventCode.Day5;

/**
 * Each line within a map contains three numbers:
 * - the destination range start,
 * - the source range start, and
 * - the range length.
 */
public class CategoryMapEntry {
    public int destinationRangeStart;
    /**
     * Exclusive
     */
    public int destinationRangeEnd;
    public int sourceRangeStart;
    /**
     * Exclusive
     */
    public int sourceRangeEnd;
    public int rangeLength;


    public CategoryMapEntry(int destination, int source, int range) {
        destinationRangeStart = destination;
        sourceRangeStart = source;
        rangeLength = range;

        destinationRangeEnd = destinationRangeStart + rangeLength;
        sourceRangeEnd = sourceRangeStart + rangeLength;
    }

    public CategoryMapEntry() {

    }
}
