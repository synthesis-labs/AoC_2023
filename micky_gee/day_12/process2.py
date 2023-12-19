import re
from tqdm import tqdm
import copy

with open('data/input.txt', 'r') as infile:
    data = infile.read().split('\n')

def score(line):
    return [len(x) for x in line.split('.') if len(x) > 0]

def generate(springs, mask):
    c = ''.join(['.' * p + '#' * s for p, s in springs[::-1]])
    c += '.' * (len(springs) - len(c))
    return ''.join([x if x != '?' else y for x, y in zip(mask, c) ])

def insert(springs, fmask, total_length):
    c = [['.'] * p + ['#'] * s for p, s in springs[::-1]]
    c = [y for x in c for y in x] #flatten lists
    c += ['.'] * (total_length - len(c))
    # print(fmask)
    # print(c)
    return fmask.format(*c)

#springs to be a reverse ordered list of (preceding periods, springs)
def increment(springs, mask, reset_space=1, max_length=None):
    if max_length is None:
        max_length = len(mask)
    # print(f'Starting increment: max_length is {max_length}')
    index = 0
    while True:
        p, s = springs[index]
        springs[index] = (p+1, s)

        length = sum([x + y for x,y in springs])
        # print(f'\tlength: {length}')
        if length > max_length:
            if index >= (len(springs) - 1):
                return None, length
            else:
                p, s = springs[index]
                springs[index] = (reset_space, s)
                index += 1
                # p, s = springs[index]
                # springs[index] = (p+1, s)
                continue
        # else:
        return springs, length

total = 0
for dat in tqdm(data):
    s,d = dat.split(' ')
    mask = ''.join([s+'?' for x in range(5)])[:-1]
    fmask = re.sub('\?', '{}', mask)
    d = [int(x) for x in re.findall('\d+', d)]
    d = d*5
    total_springs = sum(d)
    fixed_springs = len(re.findall('#', mask))
    movable_springs = total_springs-fixed_springs
    gaps = len(re.findall('\?', mask))
    # springs = [(1, x) for x in d[::-1]]
    # springs[-1] = (0, springs[-1][1])

    springs = [(0, 1) for x in range(movable_springs)]

    acc = 0
    while springs is not None:
        # g = generate(springs, mask)
        g = insert(springs, fmask, gaps)
        sg = score(g)
        # print(f'{score(g) == d}')
        if (sg == d):
            # print('MATCH MATCH MATCH!!!')
            acc += 1
        springs, ls = increment(springs, mask, reset_space=0, max_length=gaps)
        # print(f'{springs} {sum(sg)} {sum(d)}')

    print(f'Pattern {mask} found {acc} combinations')

    total += acc
