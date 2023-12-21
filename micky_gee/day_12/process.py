import re
from tqdm import tqdm
import copy

with open('data/test0.txt', 'r') as infile:
    data = infile.read().split('\n')

def score(line):
    return [len(x) for x in line.split('.') if len(x) > 0]

def permute(line, key):
    l = re.sub('\?', '{}', line)
    r = len(l) - len(line) #number of replacements or number of wildcard characters
    s = re.sub('1', '#', re.sub('0', '.', f'{key:>0{r}b}'))
    return l.format(*[x for x in s])

#part 1
# acc = 0
# for line in tqdm(data):
#     s,d = line.split(' ')
#     s = ''.join([s for x in range(1)])
#     d = [int(x) for x in re.findall('\d+', d)]
#     for k in tqdm(range(2**len(re.findall('\?', s)))):
#         if score(permute(s, k)) == d:
#             acc += 1
    # print(f'{line} --> {s} {s==d}')

def leading_periods(leadings, length):
    lp = copy.deepcopy(leadings)
    if len(leadings) == 0:
        return ''.join(['.' for d in range(length)])
    l,c = lp.pop(0)
    return ''.join(['.' for d in range(l)] + ['#' for e in range(c)]) + leading_periods(lp, length-l-c)

def reducing_sum(k, i):
    return(([k-x for x in range(i)]))

def arithmetric_series(n):
    return n*(1+n)//2

#part 2

s,d = data[0].split(' ')

def parse(line):
    s,d = line.split(' ')
    s = '?'.join([s for x in range(5)])
    d = [int(x) for x in re.findall('\d+', d)]
    d = d*5
    return s,d

def tests():
    test1()


def test1():
    s,d = parse("?#?.?## 2,2")
    print(s)
    leadings = [(1, c) for c in d]
    leadings[0] = (0, leadings[0][1])
    print(f"leadings: {leadings}")
    perms = 0
    solutions = []
    for perm in permute_strings(leadings, len(s)):
        perms += 1
        can = "".join([f"{'.' * l[0]}{'#' * l[1]}" for l in perm])
        can += '.'*(len(s)-len(can))
        # compare s to can
        solution = True
        for a,b in zip(s, can):
            if a == '#' and b != '#':
                solution = False
                break
            if a == '.' and b != '.':
                solution = False
                break
        if solution:
            solutions.append(can)
            print(can)

        # if perms % 100000 == 0:
        #     print(can )
    print(f"Total permutations {perms}")



# s = ''.join([s+'?' for x in range(5)])[:-1]
# d = [int(x) for x in re.findall('\d+', d)]
# d = d*5
#
# leadings = [(1, c) for c in d]
# leadings[0] = (0, leadings[0][1])
# print(f"leadings: {leadings}")


def permute_strings(_leadings,  length):
    leadings = copy.deepcopy(_leadings)
    done = False
    yield leadings
    while not done:
        if sum([a+b for a,b in leadings]) >= length:
            # We reached overflow
            level = -1
            # Reset the leading at 'level' to 1 and increment the superior leading
            while sum([a+b for a,b in leadings]) >= length and level > -len(leadings):
                leadings[level] = (1, leadings[level][1])
                leadings[level-1] = (leadings[level-1][0]+1, leadings[level-1][1])
                level -= 1

            if sum([a+b for a,b in leadings]) >= length:
                done = True
                # break
        else:
            leadings[-1] = (leadings[-1][0]+1, leadings[-1][1])

        # [print(f"{'.'*l[0]}{'#'*l[1]}", end="") for l in leadings]
        # print()
        # print(leadings)
        yield leadings

# leading_periods(leadings, len(s))


tests()