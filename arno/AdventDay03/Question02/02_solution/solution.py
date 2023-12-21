from itertools import product
from re import match

def read_input(file_path):
    # Read input file and remove newline characters
    with open(file_path, 'r') as file:
        rows = [row.rstrip('\n') for row in file]
    return rows

def find_potential_gears(rows):
    # Find coordinates of potential gears ('*')
    return {(i, j) for i, j in product(range(len(rows)), range(len(rows[0]))) if rows[i][j] == '*'}

def find_adjacent_digits(rows, gears):
    # Find coordinates of adjacent digits for each potential gear
    offsets = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    return [[(i + a, j + b) for a, b in offsets if rows[i + a][j + b].isnumeric()] for i, j in gears]

def filter_leftmost_digits(indices):
    # Filter only the leftmost digit for each potential gear
    return [[(i, j) for i, j in lst if (i, j - 1) not in lst] for lst in indices]

def filter_valid_gears(indices):
    # Filter gears with exactly 2 adjacent digits
    return [lst for lst in indices if len(lst) == 2]

def find_start_of_part_number(rows, indices):
    # Find the start of the part number by identifying the leftmost column
    return [[(i, min(k for k in range(j + 1) if all(s.isnumeric() for s in rows[i][k:j]))) for (i, j) in lst] for lst in indices]

def extract_part_numbers(rows, indices):
    # Extract part numbers using regular expressions
    return [[match(r"\d+", rows[i][j:]).group(0) for i, j in lst] for lst in indices]

def main():
    file_path = "/Users/arnostrydom/Dev/AoC_2023/arno/AdventDay03/Question02/01_input/input.txt"
    rows = read_input(file_path)

    gears = find_potential_gears(rows)
    print('Potential gears\n', gears)

    digit_indices = find_adjacent_digits(rows, gears)

    leftmost_digits = filter_leftmost_digits(digit_indices)
    valid_gears = filter_valid_gears(leftmost_digits)

    start_of_part_numbers = find_start_of_part_number(rows, valid_gears)
    part_numbers = extract_part_numbers(rows, start_of_part_numbers)

    print('Part numbers\n', part_numbers)
    print(sum(int(lst[0]) * int(lst[1]) for lst in part_numbers))

if __name__ == "__main__":
    main()
