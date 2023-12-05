from toolz import thread_last
import re

def read_lines_from_file(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()
    return lines

def map_triplet(triplet):
    (dest_start, source_start, length) = triplet
    return (source_start, source_start + length, dest_start - source_start)

def map_sources_to_destination(triplets):
    return [map_triplet(t) for t in triplets]

def parse_data(line):
    def split_triplet(line):
        return [int(x) for x in line.split(' ')]

    data_lines = [split_triplet(line) for line in line.split('\n')[1:]]
    return map_sources_to_destination(data_lines) 

def parse_almanac(lines):
    sections = '\n'.join(lines).split('\n\n')
    return {
        "seeds": [int(i) for i in sections[0].split(' ')[1:]],
        "seed-to-soil": parse_data(sections[1]),
        "soil-to-fertilizer": parse_data(sections[2]),
        "fertilizer-to-water": parse_data(sections[3]),
        "water-to-light": parse_data(sections[4]),
        "light-to-temperature": parse_data(sections[5]),
        "temperature-to-humidity": parse_data(sections[6]),
        "humidity-to-location": parse_data(sections[7]),
    }

def parse_seed_info(line):
    def group_list(l, n):
        return [l[i:i+n] for i in range(0, len(l), n)]

    l = [int(i) for i in line.split(' ')[1:]]
    return [[i[0], i[0] + i[1]] for i in group_list(l,2)]


def parse_almanac2(lines):
    sections = '\n'.join(lines).split('\n\n')
    return {
        "seeds_info": parse_seed_info(sections[0]),
        "seed-to-soil": parse_data(sections[1]),
        "soil-to-fertilizer": parse_data(sections[2]),
        "fertilizer-to-water": parse_data(sections[3]),
        "water-to-light": parse_data(sections[4]),
        "light-to-temperature": parse_data(sections[5]),
        "temperature-to-humidity": parse_data(sections[6]),
        "humidity-to-location": parse_data(sections[7]),
    }


def find_destination(mappings, source):
    matches = [mapping for mapping in mappings if source >= mapping[0] and source < mapping[1]]
    return source if not matches else source + matches[0][2]

def map_seeds_to_mapping(mapping, source_range):
   
    source_lower, source_upper = source_range
    (mapping_lower, mapping_upper, offset) = mapping

    # Range contained within mapping
    if source_lower >= mapping_lower and source_upper <= mapping_upper:
        return ([],[source_lower + offset, source_upper + offset])
    #range overlaps beginning of mapping
    if source_lower >= mapping_lower and source_upper >= mapping_upper and source_lower < mapping_upper:
        return ([mapping_upper + 1, source_upper],[source_lower + offset, mapping_upper + offset])
    #range overlaps end of mapping
    if source_lower <= mapping_lower and source_upper <= mapping_upper and source_upper > mapping_lower:
        return ([source_lower, mapping_lower], [mapping_lower + offset, source_upper + offset])
    
    # TODO: This needs to be implemented but it works without it just got lucky I guess
    #range overlaps mapping on both sides
    # if source_lower <= mapping_lower and source_upper >= mapping_upper:
    #     return (([source_lower, mapping_lower],[mapping_upper, source_upper]), [mapping_lower + offset, mapping_upper + offset])

    return (source_range,[])

def find_destination_on_range(mappings, source_range):
    unmapped = source_range
    ranges = []
    for mapping in mappings:
        (unmapped, mapped) = map_seeds_to_mapping(mapping, unmapped)
        if mapped:
            ranges.append(mapped)
        if not unmapped:
            return ranges

    if unmapped:
        ranges.append(unmapped)

    return ranges

def flatten(l):
    return [item for sublist in l for item in sublist]

def map_seed_to_location_ranges(almanac, seed_range):

    def lookup(name):
        return lambda r: flatten([find_destination_on_range(almanac[name], i) for i in r])

    return thread_last(
        [seed_range],
        lookup("seed-to-soil"),
        lookup("soil-to-fertilizer"),
        lookup("fertilizer-to-water"),
        lookup("water-to-light"),
        lookup("light-to-temperature"),
        lookup("temperature-to-humidity"),
        lookup("humidity-to-location")
    ) 
    
def map_seeds_to_location_ranges(almanac):
    seed_ranges = almanac["seeds_info"]
    return [ map_seed_to_location_ranges(almanac, seed_range) for seed_range in seed_ranges]

def map_seed_to_location(almanac, seed):
    def lookup(name):
        return lambda i: find_destination(almanac[name], i)

    return thread_last(
        seed,
        lookup("seed-to-soil"),
        lookup("soil-to-fertilizer"),
        lookup("fertilizer-to-water"),
        lookup("water-to-light"),
        lookup("light-to-temperature"),
        lookup("temperature-to-humidity"),
        lookup("humidity-to-location"),
    ) 

def map_seeds_to_location(almanac):
    seeds = almanac["seeds"]
    return [map_seed_to_location(almanac, seed) for seed in seeds]

def calculate_problem1(lines):
    return thread_last(
        lines,
        parse_almanac,
        map_seeds_to_location,
        min
    )
    

def calculate_problem2(lines):
    def return_range_minimum(r):
        return r[0]

    return thread_last(
        lines,
        parse_almanac2,
        map_seeds_to_location_ranges,
        flatten,
        (map, return_range_minimum),
        min
    )
    

lines = """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4""".split('\n')

lines = read_lines_from_file('input.txt')

lines = list(map(lambda l: l.strip(), lines))

print(f"problem 1: {calculate_problem1(lines)}")
print(f"problem 2: {calculate_problem2(lines)}")