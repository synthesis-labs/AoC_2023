# import itertools
# import re
# import math
import time
import copy

# import tqdm


def read_input(file='./test.txt'):
    with open(file, 'r') as inputfile:
        lines = inputfile.readlines()
    return lines


def tilt_north(block):
    # For each O, find its new position
    for col in range(1, len(block)):
        for row in range(len(block[col])):
            if block[col][row] == 'O':
                for col_2 in range(col-1, -1, -1):
                    if col_2 == 0 and block[col_2][row] == '.':
                        # Swap
                        block[col_2][row] = 'O'
                        block[col][row] = '.'
                        break
                    # Find the first # or O above it
                    elif block[col_2][row] == '#' or block[col_2][row] == 'O':
                        # Swap
                        if col_2+1 != col:
                            block[col_2+1][row] = 'O'
                            block[col][row] = '.'
                        break
    return block
#
#
# def tilt_south(block):
#     # For each O, find its new position
#     for col in range(len(block)-2, -1, -1):
#         for row in range(len(block[col])):
#             if block[col][row] == 'O':
#                 for col_2 in range(col+1, len(block)):
#                     if col_2 == len(block)-1 and block[col_2][row] == '.':
#                         # Swap
#                         block[col_2][row] = 'O'
#                         block[col][row] = '.'
#                         break
#                     # Find the first # or O above it
#                     elif block[col_2][row] == '#' or block[col_2][row] == 'O':
#                         # Swap
#                         if col_2-1 != col:
#                             block[col_2-1][row] = 'O'
#                             block[col][row] = '.'
#                         break
#     return block
#
# def tilt_east(block):
#     # For each O, find its new position
#     for col in range(len(block[0])):
#         for row in range(len(block)-1, -1, -1):
#             if block[col][row] == 'O':
#                 for row_2 in range(row+1, len(block[col])):
#                     if row_2 == len(block[col])-1 and block[col][row_2] == '.':
#                         # Swap
#                         block[col][row_2] = 'O'
#                         block[col][row] = '.'
#                         break
#                     # Find the first # or O above it
#                     elif block[col][row_2] == '#' or block[col][row_2] == 'O':
#                         # Swap
#                         if row_2-1 != row:
#                             block[col][row_2-1] = 'O'
#                             block[col][row] = '.'
#                         break
#
# def tilt_west(block):
#     # For each O, find its new position
#     for col in range(len(block[0])-1, -1, -1):
#         for row in range(len(block[col])):
#             if block[col][row] == 'O':
#                 for row_2 in range(row-1, -1, -1):
#                     if row_2 == 0 and block[col][row_2] == '.':
#                         # Swap
#                         block[col][row_2] = 'O'
#                         block[col][row] = '.'
#                         break
#                     # Find the first # or O above it
#                     elif block[col][row_2] == '#' or block[col][row_2] == 'O':
#                         # Swap
#                         if row_2+1 != row:
#                             block[col][row_2+1] = 'O'
#                             block[col][row] = '.'
#                         break
#     return block


def get_weight(tilted):
    w = 0
    for i in range(len(tilted)):
        # count the O in the row
        count = len([c for c in tilted[i] if c == 'O'])
        roww = count * (len(tilted) - i)
        # print(f"Row {i}: {count} * {len(tilted) - i} = {roww}")
        w += roww

    return w

def solve(block):
    # [print(line) for line in block]
    tilted = tilt_north(block)
    # print("Tilted:")
    # [print(line) for line in block]
    return get_weight(tilted)


def rotate_right(block):
    # Rotate the block 90 degrees clockwise
    rotated = []
    for i in range(len(block)):
        row = []
        for j in range(len(block[i])):
            row.append(block[len(block)-j-1][i])
        rotated.append(row)

    return rotated


def solve2(block, iterations):
    # [print(line) for line in block]
    # tqdm.tqdm.write("Tilting...")
    # old_block = copy.deepcopy(block)
    cycle_length = 1
    cycle_start = 0
    repeats = 0
    scores = []
    for i in range(iterations):
        block = tilt_north(block)
        west = rotate_right(block)
        block = tilt_north(west)
        south = rotate_right(block)
        block = tilt_north(south)
        east = rotate_right(block)
        block = tilt_north(east)
        block = rotate_right(block)
        score = get_weight(block)
        if score in scores:
            cycle_start = scores.index(score)
            c_length = i-cycle_start
            if c_length == cycle_length:
                repeats += 1
            else:
                cycle_length = c_length
                repeats = 1
            print(f"{i}:{score}* c_length: {c_length}, repeats: {repeats}")
        else:
            print(f"{i}:{score}")
        scores.append(score)

        if repeats == cycle_length and cycle_length > 3:
            print(f"Cycle length: {i-cycle_start} starting at {cycle_start}")
            cycle = scores[cycle_start:cycle_start+cycle_length]
            # Calculate the score for iteration number cycles
            idx = (iterations - cycle_start - 1) % cycle_length
            return cycle[idx]

    return 0

if __name__ == '__main__':

    # Test
    block = [['O', '.'], ['.', 'O']]
    tilted = tilt_north(block)
    print("Tilted:")
    [print(line) for line in tilted]
    print()
    block = [['.', '#', 'O', '.'], ['.', 'O', 'O','#'], ['O', '.', '.', 'O']]
    tilted = tilt_north(block)
    print("Tilted:")
    [print(line) for line in tilted]



    input = read_input('input.txt')
    # lines = [block.split('\n') for block in input]
    lines = [[c for c in line if c != '\n'] for line in input]
    ans = solve(lines)
    print(f"Answer 1: {ans}")
    #
    ans2 = solve2(lines, 1000000000)
    print(f"Answer 2: {ans2}")

