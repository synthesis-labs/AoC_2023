import re
from collections import Counter
from functools import cmp_to_key
import math

with open('data/test.txt', 'r') as infile:
# with open('data/input.txt', 'r') as infile:
    data = infile.read().split('\n')

hands = [(Counter(x[0]), int(x[1]), x[0]) for x in [re.findall('\S+', x) for x in data]]

# hands += [(Counter('JJJJJ'), 1, 'JJJJJ')]
# hands = [(Counter('JJJJJ'), 1, 'JJJJJ'), (Counter('99999'), 1, '99999'), (Counter('JJJJA'), 1, 'JJJJA')]

# A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2
# E, D, C, B, A, 

map = {
    'A' : 'E',
    'K' : 'D',
    'Q' : 'C',
    'J' : '1',
    'T' : 'A',
}

def sort(left, right):
    #note you may have to remove jokers for the hand comparison (this doesn't give the correct score)
    # lj = left[0].pop('J') if left[0]['J'] > 0 else 0
    # rj = right[0].pop('J') if right[0]['J'] > 0 else 0

    lj = left[0]['J']
    rj = right[0]['J']

    if lj == 5:
        left[0]['J'] = 0

    if rj == 5:
        right[0]['J'] = 0
    l = sorted(left[0].values(), reverse=True)
    r = sorted(right[0].values(), reverse=True)
    for a, b in zip(l, r):
        if a+lj > b+rj:
            return 1
        elif a+lj < b+rj:
            return -1

    for a, b in zip(left[2], right[2]):
        if a in map.keys():
            a = map[a]
        if b in map.keys():
            b = map[b]
        if a > b:
            return 1
        elif a < b:
            return -1

    return 0

hands = sorted(hands, key=cmp_to_key(sort))

score = sum([math.prod((a[1],b+1)) for a, b in zip(hands, range(len(hands)))])

print(hands)

    # if max(left[0].values()) > max(right[0].values()):
    #     return 1
    # elif max(left[0].values()) < max(right[0].values()):
    #     return -1
    # else return 0


#too high: 251653831
#          251952343
#          251699394
#          249459745
#          249139193
#          248288027
#too low:  247683962