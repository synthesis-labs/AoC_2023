import multiprocessing
import re
from multiprocessing import Process, Queue


def read_input(file='./test.txt') -> str:
    with open(file, 'r') as inputfile:
        lines = inputfile.read()
    return lines


def get_constraints(blocks):
    constraints = {}
    for block in blocks:
        lines = block.split('\n')
        constraint_name = lines[0].split(' ')[0]
        constraints[constraint_name] = []
        for line in lines[1:]:
            num = line.split(' ')
            # Recast the <dest> <source> <range> to <min> <max> <offset>
            constraints[constraint_name].append([int(num[1]), (int(num[1]) + int(num[2])), (int(num[0]) - int(num[1]))])
    return constraints


def solve_part_1(lines):
    blocks = lines.split('\n\n')

    seeds = [int(s) for s in blocks[0].split(' ')[1:]]
    print(f"Seeds: {seeds}")
    constraints = get_constraints(blocks[1:])

    # map seed to location
    locations = []
    for seed in seeds:
        print(f"Seed: {seed}")
        locations.append(do_map_seed_to_location(seed, constraints))
    # Find the smallest location
    return min(locations)


def do_map_seed_to_location(seed: int, constraints: dict[str, list[list[int]]]) -> int:
    root = seed
    soil = do_map(root, constraints['seed-to-soil'])
    fertilizer = do_map(soil, constraints['soil-to-fertilizer'])
    water = do_map(fertilizer, constraints['fertilizer-to-water'])
    light = do_map(water, constraints['water-to-light'])
    temperature = do_map(light, constraints['light-to-temperature'])
    humidity = do_map(temperature,  constraints['temperature-to-humidity'])
    location = do_map(humidity, constraints['humidity-to-location'])
    # print(f"seed: {seed}, soil: {soil}, fertilizer: {fertilizer}, water: {water}, light: {light}, temperature: {temperature}, humidity: {humidity}, location: {location}")
    return location


def do_map(source: int, mapping: list[list[int]]) -> int:
    for map in mapping:
        if map[0] <= source < map[1]:
            return source + map[2]
    return source


def do_map_multiseed_to_locations(seed_start: int, seed_range: int, constraints: dict[str, list[list[int]]], return_dict):
    smallest_location = 99999999999999999999999999
    for seed in range(seed_start, seed_start+seed_range):
        l = do_map_seed_to_location(seed, constraints)
        if l < smallest_location:
            smallest_location = l
            smallest_seed = seed
            print(f"[{seed_start}][{seed_range}] - Smallest location: {smallest_location}")
            print(f"Smallest seed: {smallest_seed}")
    return_dict[seed_start] = smallest_location


def solve_part_2(lines):
    blocks = lines.split('\n\n')
    # seeds = [int(s) for s in blocks[0].split(' ')[1:]]
    # seeds: 79 14 55 13
    matches = re.findall(r'(\d+ \d+)', blocks[0])
    seeds = []
    for m in matches:
        seeds.append( [int(m.split(' ')[0]), int(m.split(' ')[1])] )
    print(f"Seeds: {seeds}")
    constraints = get_constraints(blocks[1:])

    # use multiprocessing to spawn a process for each seed
    procs = []
    manager = multiprocessing.Manager()
    return_dict = manager.dict()
    for seed in seeds:
        proc = Process(target=do_map_multiseed_to_locations, args=(seed[0], seed[1], constraints, return_dict))
        procs.append(proc)
        proc.start()
    # wait for all processes to finish
    for proc in procs:
        proc.join()

    return min(return_dict.values())

if __name__ == '__main__':
    lines = read_input('input1.txt')
    answ1 = solve_part_1(lines)
    print(f"Answer 1: {answ1}")

    answ2 = solve_part_2(lines)
    print(f"Answer 2: {answ2}")
