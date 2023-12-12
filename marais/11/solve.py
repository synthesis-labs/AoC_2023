from typing import List, Tuple

import numpy as np
def read_input(file='./test.txt'):
    with open(file, 'r') as inputfile:
        lines = inputfile.readlines()
        lines = [line.strip() for line in lines]
    return lines


# Find the pairs of coordinates of the # characters
def find_pairs(lines: list[[str]]) -> list[tuple[tuple[int, int], tuple[int, int]]]:
    galaxies = set()
    pairs = []
    for r in range(len(lines)):
        for c in range(len(lines[r])):
            if lines[r][c] == '#':
                galaxies.add((r, c))
    while galaxies:
        a = galaxies.pop()
        for b in galaxies:
            pairs.append((a, b))
    return pairs


def manhattan_distance(pair: tuple[tuple[int, int], tuple[int, int]], empty_rows: list[int], empty_cols, factor) -> int:
    row_dist = abs(pair[0][0] - pair[1][0])
    col_dist = abs(pair[0][1] - pair[1][1])
    min_r = min(pair[0][0], pair[1][0])
    max_r = max(pair[0][0], pair[1][0])
    min_c = min(pair[0][1], pair[1][1])
    max_c = max(pair[0][1], pair[1][1])
    # print(f"Pair: {pair}")
    # print(f"Row dist: {row_dist}")
    # print(f"Col dist: {col_dist}")
    add_rows = sum([1 if min_r < r < max_r else 0 for r in empty_rows])
    # print(f"Add rows: {add_rows}")
    add_cols = sum([1 if min_c < c < max_c else 0 for c in empty_cols])
    # print(f"Add cols: {add_cols}")
    return row_dist + col_dist - add_rows - add_cols + add_rows * factor + add_cols * factor


def find_empty(lines):
    empty_rows = []
    empty_cols = []
    for r in range(len(lines)):
        if all([c == '.' for c in lines[r]]):
            empty_rows.append(r)
    for c in range(len(lines[0])):
        if all([lines[r][c] == '.' for r in range(len(lines))]):
            empty_cols.append(c)
    return empty_rows, empty_cols


if __name__ == '__main__':
    lines = read_input('input.txt')
    # [print(line) for line in lines]
    empty_rows, empty_cols = find_empty(lines)
    # print(f"Empty rows: {empty_rows}")
    # print(f"Empty cols: {empty_cols}")
    pairs = find_pairs(lines)
    distances = [manhattan_distance(pair, empty_rows, empty_cols, 1) for pair in pairs]
    # print(distances)
    # [print(line) for line in lines]
    print(f"Answer 1: {sum(distances)}")
    distances2 = [manhattan_distance(pair, empty_rows, empty_cols, 1000000) for pair in pairs]
    # print(distances2)
    print(f"Answer 2: {sum(distances2)}")

