import math
import os
import sys
import time

def read_file(input_file_path: str):
        # Add os.path.join(sys.path[0] to make it a relative path
        with open(os.path.join(sys.path[0], input_file_path), "r", encoding="utf-8") as input_file:
            text = input_file.read().strip()
        lines = text.split("\n")
        return lines

class SeedMap:
    def __init__(self, source_category: str, destination_category: str):
        self.source_category = source_category
        self.destination_category = destination_category
        self.mapping_ranges: list[tuple[int, int, int]] = []

    # Mapping ranges:
    def add_mapping_range(self, destination_range_start: int, source_range_start: int, range_length: int):
        # Add a mapping range to the seed map
        self.mapping_ranges.append((source_range_start, source_range_start + range_length, destination_range_start))

    def get_destination(self, given: int):
        # Map the given value through all mapping ranges
        # e.g. The first line has a destination range start of 50, a source range start of 98, and a range length of 2.
        for source_range_start, max_source, destination_range_start in self.mapping_ranges:
            # If the given value is in the current range, map it
            if source_range_start <= given < max_source:
                return destination_range_start + (given - source_range_start)
        # If no range was found, return the value unchanged
        return given
    

def main():
    # Read input text from file
    lines = read_file("../01_input/input.txt")

    # Get the seed numbers - almanac first line
    first_line = lines[0]
    seed_values = first_line.split(": ")[1].split(" ")
    seeds = [int(s) for s in seed_values]

    # Almanac lines 2 to end - list of maps which describe how to convert numbers from a 
    # "source category" into numbers in a "destination category"
    # Create seed maps
    all_seed_maps: list[SeedMap] = []
    current_map = None

    # Iterate through each line except the first one
    for line in lines[1:]:
        if not line.strip():
            continue
        # Marker = map: -> When line ends with map, the new map is starting
        if line.endswith("map:"):
            # Get source_category and destination_range_start_range_start prefixes for the current map
            source_category, destination_category = line.split(" ")[0].split("-to-", 1)
            # Create a new map for the current map e.g. seed-to-soil map:
            current_map = SeedMap(source_category, destination_category)
            # Add it to the list of seed maps
            all_seed_maps.append(current_map)
        else:
            # Add all numbers to the current map mapping ranges
            # With this map, you can look up the soil number required for each initial seed number:
            # Seed number 79 corresponds to soil number 81.
            current_map.add_mapping_range(*[int(s) for s in line.split(" ")])

    min_location = math.inf

    # For each seed
    for seed in seeds:
        # Map it through all seed maps
        for seed_map in all_seed_maps:
            seed = seed_map.get_destination(seed)
        # Update the minimum location
        min_location = min(min_location, seed)

    print(min_location)

if __name__ == "__main__":
    main()