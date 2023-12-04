from toolz import thread_last
import re

def read_lines_from_file(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()
    return lines


def get_neighbor(lines, x, y):
    if y < 0 or x < 0 or y >= len(lines) or x >= len(lines[0]) :
        return '.'
    else:
        return lines[y][x]
    

def get_neighborhood(lines, x,y):
    ans = [
        [get_neighbor(lines, x-1, y-1), get_neighbor(lines, x, y-1), get_neighbor(lines, x+1, y-1)],
        [get_neighbor(lines, x-1, y), get_neighbor(lines, x, y), get_neighbor(lines, x+1, y)],
        [get_neighbor(lines, x-1, y+1), get_neighbor(lines, x, y+1), get_neighbor(lines, x+1, y+1)]
    ]
    return ans

def get_neighbors(lines, x,y):
    ans = [
        [get_neighbor(lines, x-1, y-1), get_neighbor(lines, x, y-1), get_neighbor(lines, x+1, y-1)],
        [get_neighbor(lines, x-1, y), '.', get_neighbor(lines, x+1, y)],
        [get_neighbor(lines, x-1, y+1), get_neighbor(lines, x, y+1), get_neighbor(lines, x+1, y+1)]
    ]
    return ans

def is_symbol(symbol):
    return not is_number(symbol) and symbol not in [' ', '.']

def is_number(symbol):
    return symbol in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] or type(symbol) == int

def is_part_of_number(neighbors):
    return is_number(neighbors[1][1])

def is_a_symbol(neighbors):
    return is_symbol(neighbors[1][1])

def is_a_symbol(neighbors):
    return neighbors[1][1] == '*'


def clean(n):
    return n and n != '.'

def touches_alive_and_is_number(nums,alive):
    return nums[1][1] and (clean(alive[1][0]) or clean(alive[1][1]) or clean(alive[1][2]))

def neighbors_are_symbols(neighbors):
    for line in neighbors:
        for char in line:
           if is_symbol(char):
               return True
    return False


def connected_to_gear(numbers, gears):
    if not numbers[1][1]:
        return False

    for line in gears:
        for gear in line:
            if gear == True:
               return True
    return False

def get_gear_ratios(neighbors, gears):
    if not clean(gears[1][1]):
        return None

    g = []

    #Top
    if is_number(neighbors[0][0]) and is_number(neighbors[0][1]) and is_number(neighbors[0][2]):
        g.append(neighbors[0][1])
    elif is_number(neighbors[0][0]) and not is_number(neighbors[0][1]) and is_number(neighbors[0][2]):
        g.append(neighbors[0][0])
        g.append(neighbors[0][2])
    elif is_number(neighbors[0][0]):
        g.append(neighbors[0][0])
    elif is_number(neighbors[0][2]):
        g.append(neighbors[0][2])
    elif is_number(neighbors[0][1]):
        g.append(neighbors[0][1])

    #Bottom
    if is_number(neighbors[2][0]) and is_number(neighbors[2][1]) and is_number(neighbors[2][2]):
        g.append(neighbors[2][1])
    elif is_number(neighbors[2][0]) and not is_number(neighbors[2][1]) and is_number(neighbors[2][2]):
        g.append(neighbors[2][0])
        g.append(neighbors[2][2])
    elif is_number(neighbors[2][0]):
        g.append(neighbors[2][0])
    elif is_number(neighbors[2][2]):
        g.append(neighbors[2][2])
    elif is_number(neighbors[2][1]):
        g.append(neighbors[2][1])

    #Middle
    if is_number(neighbors[1][0]) and is_number(neighbors[1][2]):
        g.append(neighbors[1][0])
        g.append(neighbors[1][2])
    elif is_number(neighbors[1][0]):
        g.append(neighbors[1][0])
    elif is_number(neighbors[1][2]):
        g.append(neighbors[1][2])

    if len(g) != 2:
        print("Error in get_gear_ratios", g)

    return g[0]*g[1]

def total_numbers(lines, alive):
    # print(alive)
    if (clean(alive[1][0]) and clean(alive[1][1]) and clean(alive[1][2])):
        return int(''.join(lines[1]))

    if (not clean(alive[1][0]) and clean(alive[1][1]) and not clean(alive[1][2])):
        return int(lines[1][1])

    if not clean(alive[1][0]) and clean(alive[1][1] ) and clean(alive[1][2]):
        return int(lines[1][1] + lines[1][2])
    
    if (clean(alive[1][0]) and clean(alive[1][1]) and not clean(alive[1][2])):
        return int(lines[1][0] + lines[1][1])

    # return 0

def max_m(m):
    if type(m[1][1]) == int:
        return max([i for i in m[1] if type(i) == int])

def is_a_gear(neighbors):
    if not neighbors[1][1] == '*':
        return False

    # print(neighbors)
    sides = 0
    #Top
    if is_number(neighbors[0][0]) and not is_number(neighbors[0][1]) and is_number(neighbors[0][2]):
        sides+=2
    elif is_number(neighbors[0][0]) or is_number(neighbors[0][1]) or is_number(neighbors[0][2]):
        sides+=1
    
    #Mid
    if is_number(neighbors[1][0]):
        sides+=1
    if is_number(neighbors[1][2]):
        sides+=1

    #Bottom
    if is_number(neighbors[2][0]) and not is_number(neighbors[2][1]) and is_number(neighbors[2][2]):
        sides+=2
    elif is_number(neighbors[2][0]) or is_number(neighbors[2][1]) or is_number(neighbors[2][2]):
        sides+=1

    return sides==2

def convolution(lines, mask_func, function):
    height = len(lines)
    width = len(lines[0])

    result = []

    for y in range(height):
        row = []
        for x in range(width):
            n = get_neighborhood(lines, x, y)
            row.append(function(n))
        result.append(row)

    return result

def convolution_neighborhoods(m1, m2, function):
    height = len(lines)
    width = len(lines[0])

    result = []

    for y in range(height):
        row = []
        for x in range(width):
            n1 = get_neighborhood(m1, x, y)
            n2 = get_neighborhood(m2, x, y)
            row.append(function(n1, n2))
        result.append(row)

    return result

def convolution_neighbors(lines, function):
    return convolution(lines, get_neighbors, function)

def convolution_neighborhood(lines, function):
    return convolution(lines, get_neighborhood, function)

def find_broken_parts(lines):
    broken_parts = []

    return broken_parts

def mask_matrices(input_matrix, mask):     
    return add_matrices(input_matrix, mask, lambda i,m: i if m else ' ')

def add_matrices(m1, m2, function):
    height = len(m1)
    width = len(m1[0])

    result = []

    for y in range(height):
        row = []
        for x in range(width):
            row.append(function(m1[y][x], m2[y][x]))
        result.append(row)

    return result

def map_matrix(input_matrix, function):
    height = len(input_matrix)
    width = len(input_matrix[0])

    result = []

    for y in range(height):
        row = []
        for x in range(width):
            row.append(function(input_matrix[y][x]))
        result.append(row)

    return result


def extract_numbers(matrix):
    results = []
    for r in matrix:
        t = [int(n) for n in ''.join(r).split(' ') if n != '']
        for p in t:
            results.append(p)
    return results

def print_matrix(matrix):
    for row in matrix:
        print(','.join(map(str,row)))

def calculate_problem1(lines):
    touches_a_symbol = convolution_neighbors(lines, neighbors_are_symbols)

    is_a_number = convolution_neighborhood(lines, is_part_of_number)

    alive_1 = add_matrices(touches_a_symbol, is_a_number, lambda x,y: x and y)

    touches_alive_m = convolution_neighborhoods(is_a_number, alive_1, touches_alive_and_is_number)
    touches_alive_m = convolution_neighborhoods(is_a_number, touches_alive_m, touches_alive_and_is_number)
    
    alive_and_number = add_matrices(touches_alive_m, is_a_number, lambda x,y: (x and y))
    mask_matrices_m = mask_matrices(lines, alive_and_number)

    result = map_matrix(mask_matrices_m, lambda x: ' ' if x == '.' else x)
    
    results = extract_numbers(result)
  
    return sum(results)

def calculate_problem2(lines):
    def show(m):
        print_matrix(mask_matrices(lines, m))

    is_a_gear_m = convolution_neighborhood(lines, is_a_gear)
    is_a_number = convolution_neighborhood(lines, is_part_of_number)

    connected_numbers = convolution_neighborhoods(is_a_number, is_a_gear_m, connected_to_gear)

    touches_alive_m = convolution_neighborhoods(is_a_number, connected_numbers, touches_alive_and_is_number)
    touches_alive_m = convolution_neighborhoods(is_a_number, touches_alive_m, touches_alive_and_is_number)
    
    total_nums = convolution_neighborhoods(lines, touches_alive_m, total_numbers)
    max_ = convolution_neighborhood(total_nums, max_m)

    gear_ratios = convolution_neighborhoods(max_, is_a_gear_m, get_gear_ratios)
    
    results = []
    for r in gear_ratios:
        for p in r:
            if p:
                results.append(p)
  
    return sum(results)

lines = """@..........8
...*........
460.473.....
............
..........62
75#......*..
.......478..""".split('\n')
# lines = read_lines_from_file('input.txt')

lines = list(map(lambda l: l.strip(), lines))
print(f"problem 1: {calculate_problem1(lines)}")
print(f"problem 2: {calculate_problem2(lines)}")