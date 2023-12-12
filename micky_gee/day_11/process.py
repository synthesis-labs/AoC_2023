import re
import numpy as np
import itertools

np.set_printoptions(linewidth=200)

with open('data/input.txt', 'r') as infile:
    data = infile.read().split('\n')

data = np.char.array([[x for x in y] for y in data])

row_space = []
for row in range(data.shape[0]):
    if len(np.unique(data[row])) == 1:
        row_space += [row]

col_space = []
for col in range(data.shape[0]):
    if len(np.unique(data[:,col])) == 1:
        col_space += [col]

def manhattan(a, b, factor):
    rows = len([x for x in row_space if min(a[0], b[0]) < x < max(a[0], b[0])])
    cols = len([x for x in col_space if min(a[1], b[1]) < x < max(a[1], b[1])])
    return sum(abs(val1-val2) for val1, val2 in zip(a,b)) + (factor-1)*(rows + cols)

# for row in range(data.shape[0] - 1, 0, -1):
#     if len(np.unique(data[row])) == 1:
#         data = np.insert(data, row, data[row], axis=0)

# for col in range(data.shape[1] - 1, 0, -1):
#     if len(np.unique(data[:,col])) == 1:
#         data = np.insert(data, col, data[:,col], axis=1)

galaxies = [tuple(x) for x in np.argwhere(data.find('#') == 0)]

acc = 0
for a,b in itertools.combinations_with_replacement(galaxies, 2):
    d = manhattan(a, b, 1e6)
    print(f'{a}-{b}: {d}')
    acc += d