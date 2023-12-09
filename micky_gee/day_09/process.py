import re
import numpy as np

np.set_printoptions(linewidth=np.inf)

with open('data/input.txt', 'r') as infile:
    data = infile.read().split('\n')

def find_next(arr):
    if np.all(list(map(lambda x: x == 0, arr))):
        return 0
    else:
        return arr[-1] + find_next(np.diff(arr))

acc = 0

for entry in data:
    d = [int(x) for x in re.findall('-?\d+', entry)][::-1]
    r = find_next(d)
    acc += r
