import re
import numpy as np

with open('data/test_3.txt', 'r') as infile:
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

