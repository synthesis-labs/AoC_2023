import re
from tqdm import tqdm
import itertools

with open('data/test.txt', 'r') as infile:
    data = infile.read().split('\n\n')

# seeds = [x for x in re.findall('\d+\s\d+', data[0])]
seeds = [x for x in re.findall('\d+', data[0])]

def make_map(input):
    m = [[int(x) for x in re.findall('\d+', y)] for y in input.split('\n')[1:]]
    return m

maps = [make_map(x) for x in data[1:]]

def trace_map(source_id, map):
    #m is a tuple of (dest, source, width)
    for m in map:
        start = m[1]
        end = m[1] + m[2] - 1
        if start <= source_id <= end:
            # print('.', end='')
            return m[0] + (source_id - start)
    # print('*', end='')
    return source_id

##this function takes a source range (start, width) and returns a list of maps for that range
def merge_map(first, second):
    #every entry is a tuple of (dest, source, width)
    print(f'0{first} - 1{second}', end='')
    # print(second)
    d0 = first[0]
    s0 = first[1]
    r0 = first[2]
    d1 = second[0]
    s1 = second[1]
    r1 = second[2]

    if s1 <= d0:
        #cases 1, 2, 5
        if (s1 + r1) < (d0):
            #case 1 (no common overlap between maps)
            print('case 1')
            val = []
        elif (s1+r1) < (d0 + r0):
            #case 2 (minor offset overlap )
            print('case 2')
            val = [d1 + d0 - s1, s0, s1 + r1 - d0+2]
        else:
            #case 5 (second map source bigger than first map destination)
            print('case 5')
            val = [d1 + d0 - s1, s0, r0]
    elif s1 <= (d0 + r0):
        #cases 3, 4
        if (s1 + r1) > (d0 + r0):
            #case 3 (second map source offset past first map destination)
            print('case 3')
            val = [d1, s0 + s1 - d0, d0 + r0 - s1]
        else:
            #case 4 (second map source contained by first map destination)
            print('case 4')
            val = [d1, s0 + s1 - d0, r1]
    else:
        #case 6
        print('case 6')
        val = []

    print(f'  -->  {val}')
    return val
        

# seeds = [re.findall('\d+', x) for x in seeds]
# seeds = [[int(x[0]), int(x[0]), int(x[1])] for x in seeds]

source = maps[0]
# source = seeds
for dest in maps[1]:
    newmap = []
    for entry in source:
        newmap += [merge_map(entry, x) for x in [dest]]
    newmap = [x for x in newmap if len(x) > 0]
    source = newmap

# source.sort(key=lambda x: x[1])


#part 1
locations = None
for seed in (seeds):
    print(f'{seed} -> ', end='')
    seed = trace_map(int(seed), source)
    print(f'{seed}')
    if locations is None:
        locations = seed
    else:
        locations = min(locations, seed)

# #part 2
# #This approach is almost intractable, found correct answer as 125742456
# locations = None
# for seed in tqdm(seeds):
#     first, second = re.findall('\d+', seed)
#     for a in tqdm(range(int(second))):
#         seed = int(first) + a
#         for m in maps:
#             print(f'{seed} -> ', end='')
#             seed = trace_map(seed, m)
#         print(seed)
#         if locations is None:
#             locations = seed
#         else:
#             locations = min(locations, seed)

