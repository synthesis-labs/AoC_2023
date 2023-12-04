import re
import numpy as np

with open('data/input.txt', 'r') as infile:
    data = infile.read().split('\n')

vals = np.ones((len(data)))
score=0
for card, i in zip(data, range(len(data))):
    m = re.match('Card\s+(\d+):\s(.+)\s\|\s(.+)', card)
    winning = set(m.group(2).split())
    numbers = set(m.group(3).split())
    # print(m)
    # print(winning.intersection(numbers))
    # print(len(winning.intersection(numbers)))
    if len(winning.intersection(numbers)) > 0:
        # print('score :', end='')
        score += 2**(len(winning.intersection(numbers)) - 1)

    y = len(winning.intersection(numbers))
    x = vals[i]
    vals[i+1:i+y+1] = vals[i+1:i+y+1] + x
    print(vals)


