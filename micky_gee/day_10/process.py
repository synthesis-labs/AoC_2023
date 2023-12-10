import re
import numpy as np
np.set_printoptions(linewidth=np.inf)
from tqdm import tqdm

with open('data/input.txt', 'r') as infile:
    data = infile.read().split('\n')

data = np.char.array([[x for x in y] for y in data])

location = np.unravel_index(data.find('S').argmax(), data.shape)

#direction (row, column)
def step(direction, character):
    match character:
        case '-':
            if direction == (0, 1):
                return (0, 1)
            elif direction == (0, -1):
                return (0, -1)
            else:
                return None # raise Exception("Invalid approach to '-'")
        case '|':
            if direction == (1, 0):
                return (1, 0)
            elif direction == (-1, 0):
                return (-1, 0)
            else:
                return None # raise Exception("Invalid appraoch to '|'")
        case 'L':
            if direction == (1, 0):
                return (0, 1)
            elif direction == (0, -1):
                return (-1, 0)
            else:
                return None # raise Exception("Invalid approach to 'L'")
        case 'J':
            if direction == (1, 0):
                return (0, -1)
            elif direction == (0, 1):
                return (-1, 0)
            else:
                return None # raise Exception("Invalid approach to 'L'")
        case '7':
            if direction == (-1, 0):
                return (0, -1)
            elif direction == (0, 1):
                return (1, 0)
            else:
                return None # raise Exception("Invalid approach to 'L'")
        case 'F':
            if direction == (0, -1):
                return (1, 0)
            elif direction == (-1, 0):
                return (0, 1)
            else:
                return None # raise Exception("Invalid approach to 'L'")
    return None # raise Exception("Non-tube character")
    
def print_map(map, location):
    f = map.copy()
    f[location] = '*'
    print(f)

def add(x, y):
    return tuple(map(sum, zip(x, y)))

# location (row, column)
# location = add(location, (0, 1))
# direction = (0, 1)

# directions = [(-1, 0), (1, 0)] #for input
# directions = [(1, 0), (0, 1)]
directions = [(1, 0)]
locations = [location for x in range(len(directions))]
coords = [location]

count = 1
# for iteration in range(10):
while all([x is not None for x in directions]):
    for direction, location, i in zip(directions, locations, range(len(directions))):
        # s = step(direction, data[location])
        # print(f'Moving {s} from {location} on {data[location]}')
        location = add(location, direction)
        direction = step(direction, data[location])
        data[location] = '*'
        print(f'{count}: Arriving at {location}, moving {direction} on {data[location]}.')
        directions[i] = direction
        locations[i] = location
        coords += [location]

        print_map(data, location)
        if direction is None:
            break
    count += 1

# https://stackoverflow.com/questions/3838329/how-can-i-check-if-two-segments-intersect
# https://bryceboe.com/2006/10/23/line-segment-intersection-algorithm/
def ccw(A,B,C):
    return (C[0]-A[0]) * (B[1]-A[1]) > (B[0]-A[0]) * (C[1]-A[1])

# Return true if line segments AB and CD intersect
def intersect(A,B,C,D):
    return ccw(A,C,D) != ccw(B,C,D) and ccw(A,B,C) != ccw(A,B,D)

import itertools
def in_hull(point, hull):
    A = (0, 0)
    B = point
    inters = sum([intersect(A, B, C, D) for C, D in itertools.pairwise(coords)]) % 2
    return inters != 0

# b = [tuple(x) for x in np.argwhere(data.find('.') == 0)]

# for p in b:
s = data.shape
for row in tqdm(range(s[0])):
    for col in tqdm(range(s[1])):
        p = (row, col)
        if p in coords:
            print(f'I''m in coords')
            continue
        if in_hull(p, coords):
            data[p] = 'I'
        else:
            data[p] = 'O'