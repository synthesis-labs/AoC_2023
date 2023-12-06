import os
import sys
from operator import itemgetter
from typing import List

def read_file(input_file_path: str):
    # Read the content of the file and split it into lines
    with open(os.path.join(sys.path[0], input_file_path), "r", encoding="utf-8") as input_file:
        text = input_file.read().strip()
    lines = text.split("\n")
    return lines

class SeedMap:
    def __init__(self, source_prefix: str, target_prefix: str):
        self.source_prefix = source_prefix
        self.target_prefix = target_prefix
        # Mapping ranges (source, max_source, target)
        self.mapping_ranges: List[tuple[int, int, int]] = []

    def add_mapping_range(self, target: int, source: int, offset: int):
        # Add a mapping range to the seed map
        self.mapping_ranges.append((source, source + offset, target))

    def sort(self):
        # Sort the mapping ranges by the starting point
        self.mapping_ranges.sort(key=itemgetter(0))

    def get_destination(self, ranges: "SeedRangeList"):
        new_ranges = SeedRangeList()
        
        # For each range of seeds
        for start, end in ranges:
            # If we fully mapped the range, we can continue with the next range
            found_mapping = False
            
            # For each mapping range
            for source, max_source, target in self.mapping_ranges:
                # If start is before the mapping range
                if start < source:
                    # If end is before the mapping range
                    if end < source:
                        new_ranges.add(start, end)
                        found_mapping = True
                        break
                    
                    if end <= max_source:
                        new_ranges.add(start, source)
                        new_ranges.add(target, target + (end - source))
                        found_mapping = True
                        break
                    
                    new_ranges.add(start, source)
                    new_ranges.add(target, target + (max_source - source))
                    start = max_source
                
                elif source <= start < max_source:
                    if end <= max_source:
                        new_ranges.add(target + (start - source), target + (end - source))
                        found_mapping = True
                        break
                    
                    new_ranges.add(target + (start - source) , target + (max_source - source))
                    start = max_source
                
            if not found_mapping:
                new_ranges.add(start, end)
        
        return new_ranges

class SeedRangeList(List):
    def __init__(self):
        # The values on the initial seeds: line come in pairs.
        # Within each pair, the first value is the start of the range and the second value is the length of the range.
        self.ranges: List[tuple[int, int]] = []

    def add(self, start: int, end: int):
        for current_range, (s, e) in enumerate(self.ranges):
            if start < s:
                # Add the ranges in order
                if end < s:
                    self.ranges.insert(current_range, (start, end))
                    return
                if end <= e:
                    self.ranges.insert(current_range, (start, s))
                    return
                self.ranges.insert(current_range, (start, s))
                start = e
            elif start <= s <= end:
                if end <= e:
                   return
                start = e
        # Return the remaining range
        self.ranges.append((start, end))

    def __iter__(self):
        return iter(self.ranges)

def main():
    # Read input file
    lines = read_file("../01_input/input.txt")

    # Get the seed numbers - almanac first line
    seed_range = lines[0].split(": ")[1].split(" ")
    
    seeds = SeedRangeList()
    
    # For each pair of seed inputs
    for i in range(0, len(seed_range), 2):
        # Add the range to the list
        seeds.add(int(seed_range[i]), int(seed_range[i]) + int(seed_range[i+1]))

    # Create a list of seed maps
    seed_maps: List[SeedMap] = []

    current_map = None

    for line in lines[1:]:
        # Ignore empty lines
        if not line.strip():
            continue

        # New map starts
        if line.endswith("map:"):
            off, to = line.split(" ")[0].split("-to-", 1)
            current_map = SeedMap(off, to)
            seed_maps.append(current_map)
        else:
            current_map.add_mapping_range(*[int(s) for s in line.split(" ")])

    for seed_map in seed_maps:
        seed_map.sort()

    for seed_map in seed_maps:
        # Map all seed ranges to a new destination
        seeds = seed_map.get_destination(seeds)

    # ANSWER ==> Minimum location (start of a range)
    print(min([start for start, _ in seeds]))

if __name__ == "__main__":
    main()
